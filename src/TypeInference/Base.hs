{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module contenant les primitives pour le système de type
module TypeInference.Base where 

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Monoid
import Data.Function
import TypeInference.TypeVariable
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import GHC.Exts
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))
--http://hackage.haskell.org/package/sized-vector-1.4.3.1/docs/Data-Vector-Sized.html

type Inference m a = StateT TypeVariable (ExceptT String m) a

--TODO Ajouter TApp [(Int, Monotype)] ou bien TApp Monotype [Monotype], où le Int est le nombre de parametres. 
-- | Un type pouvant être soit une variable, soit une constante, soit une fonction (arrow)
data Monotype = TVar TypeVariable
    | TConstant String
    --Arrow pourrait être quelque chose comme TApp [(TConstant "->"), type1, type2]
    | Arrow Monotype Monotype deriving (Show, Eq, Ord)
    

    
instance Pretty Monotype where
    pretty (TVar t) = pretty t
    pretty (TConstant t) = text t
    pretty (Arrow t1 t2) = parens (pretty t1 <+> text "=>" <+> pretty t2)
    
--getLambdaValue x expr = createArrow (getValue x) (getExprValue expr)
    
-- | Crée un type fonction
createArrow :: Monotype -> Monotype -> Monotype
createArrow = Arrow

getArrow :: Monotype -> Maybe (Monotype, Monotype)
getArrow (Arrow a b) = Just (a, b)
getArrow _ = Nothing
    
--forall a => a -> Int == Polytype ['a'] (Arrow (TVar "a") (TVar "b"))
--a est bound et b est libre (free)
-- | Représente un type polymorphique
data Polytype = Polytype [TypeVariable] Monotype deriving (Show)

instance Pretty Polytype where
    pretty (Polytype alpha t) = text "forall" <+> cat (punctuate comma (fmap pretty alpha)) <+> char '.' <//> pretty t

toPolytype :: Monotype -> Polytype
toPolytype mono = Polytype [] mono



-- |Map une fonction a un type
newtype TypeEnvironment = TypeEnvironment (Map.Map String Polytype) deriving (Show, Monoid)

instance Pretty TypeEnvironment where
    pretty (TypeEnvironment env) = vcat $ line <$> Map.toList env
        where 
            line (name, t) = brackets (text name <+> text ":=" <+> pretty t)

emptyEnv = TypeEnvironment mempty

instance IsList TypeEnvironment where
  type Item TypeEnvironment = (String,Polytype)
  fromList = TypeEnvironment . Map.fromList
  toList (TypeEnvironment env)  = Map.toList env

-- | Trouve le type d'un environment avec une variable
findType :: Monad m => String -> TypeEnvironment -> Inference m Polytype
findType t e@(TypeEnvironment env) = 
    case Map.lookup t env of
        Just x -> return x
        Nothing -> lift $ throwE $ "Cannot find " ++ show t ++ " in the typing environment " ++ (show $ pretty e)
-- |Map des variables à un type
newtype Substitutions = Substitutions (Map.Map TypeVariable Monotype) deriving (Show)

instance Monoid Substitutions where
    mappend (Substitutions s1) s2@(Substitutions s2M) = Substitutions $ s1M <> s2M
        where 
            s1M = Map.map (substitute s2) s1
    mempty = Substitutions $ Map.empty

extends :: TypeVariable -> Monotype -> Substitutions -> Substitutions
extends k v (Substitutions s) = Substitutions $ Map.insert k v s

-- |Trouve les variables libre dans une structure de données
class FreeTypeVariable a where
    -- In computer programming, the term freeVariables variable refers to variables used in a function that are neither local variables nor parameters of that function
    freeVariables :: a -> Set.Set TypeVariable
    
-- | Subtitue les variables dans une expression
class Subtitute a where
    substitute :: Substitutions -> a -> a
    
instance FreeTypeVariable Polytype where
    freeVariables (Polytype bounds t) = freeVariables t `Set.difference` Set.fromList bounds
    
instance FreeTypeVariable a => FreeTypeVariable [a] where
    freeVariables xs = mconcat $ fmap freeVariables xs 
    
instance FreeTypeVariable Monotype where
    freeVariables (TConstant _)       = Set.empty
    freeVariables (TVar a)       = Set.singleton a
    freeVariables (t1 `Arrow` t2) = freeVariables t1 <> freeVariables t2
    
instance FreeTypeVariable TypeEnvironment where
    freeVariables (TypeEnvironment env) = freeVariables $ Map.elems env
-- |Transforme un type en un autre, c'est à dire que si on a une fonction a -> b, et qu'on a deviné que a = Int, substitute va retourner Int -> b
instance Subtitute Monotype where
    
    substitute _ (TConstant a)       = TConstant a
    --[x/e']y = e' if x == y, else y
    --Si on ne trouve pas de remplacement, on retourne la valeur initial
    substitute (Substitutions s) t@(TVar a)     = Map.findWithDefault t a s
    --[x/e′](e1e2) = ([x/e′] e1)([x/e′]e2)
    substitute s (t1 `Arrow` t2) = createArrow (substitute s t1) (substitute s t2)

instance Subtitute Polytype where
    substitute (Substitutions s) (Polytype bounds monotype) = 
        Polytype bounds $ substitute (Substitutions (foldr Map.delete s bounds)) monotype
    
instance Subtitute TypeEnvironment where
    substitute s (TypeEnvironment env) = TypeEnvironment $ Map.map (substitute s) env



unifyWithSubstitutions :: Monad m => Substitutions -> Monotype -> Monotype -> Inference m Substitutions
unifyWithSubstitutions subs = unify `on` (substitute subs)

-- | Trouve toutes les subtitutions entre deux types
unify :: Monad m => Monotype -> Monotype -> Inference m Substitutions
unify (Arrow l r) (Arrow l' r') = do
    subs1 <- unify l l'
    subs2 <- unifyWithSubstitutions subs1 r r'
    return (subs1 <> subs2)
unify (TVar a) b = unifyTypeVar a b
unify b (TVar a) = unifyTypeVar a b

unify (TConstant a) (TConstant b)
    |a == b = return mempty
    
unify a b = lift $ throwE $ "Monotype error on unifying type " ++ (show a) ++ " and type " ++ (show b) 

unifyTypeVar :: Monad m => TypeVariable -> Monotype -> Inference m Substitutions
unifyTypeVar a t@(TVar b)
   |a == b = return mempty
   |Set.member a $ freeVariables t = lift $ throwE $ "Infinite type " ++ show a 
unifyTypeVar a b = return $ Substitutions (Map.singleton a b)