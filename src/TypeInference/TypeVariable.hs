{-# LANGUAGE FlexibleContexts #-}
-- | S'occupe du nommage des variables de type
module TypeInference.TypeVariable (TypeVariable(..), newTypeVariable, baseVariable) where
import Control.Monad.State.Class
import Text.PrettyPrint.ANSI.Leijen

-- | Variable de type
newtype TypeVariable =  TypeVariable String
    deriving (Show, Eq, Ord)
    
instance Pretty TypeVariable where
    pretty (TypeVariable v) = text v
    
toEnumImpl 0 = []
toEnumImpl i = (['a'..'z'] !! rem) : toEnumImpl val
    where 
        i' = i - 1
        rem = i' `mod` 26
        val = (i' - rem) `div` 26

letterIndex a = fromEnum a - fromEnum 'a'
        
instance Enum TypeVariable where
    toEnum i = TypeVariable $ reverse $ toEnumImpl (i + 1)
    fromEnum (TypeVariable str) = (foldr (\x acc -> acc * 26 + x) 0 $ map letterIndex str)

-- | Instancie une nouvelle variable avec un nom unique
newTypeVariable :: MonadState TypeVariable m => m TypeVariable
newTypeVariable = do
    v <- get
    put (succ v :: TypeVariable)
    return v
    
baseVariable = TypeVariable "a"