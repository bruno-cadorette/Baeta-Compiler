{-# LANGUAGE DeriveFunctor #-}
-- | Module pour faire la gestion des modules et des dépendances entre modules. Utilise des algorithmes sur les graphes
module ModuleSystem (ParsedModule(..), createValidGraph, ModuleError(..), Module, mapModuleM, joinModules) where
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Foldable
import Data.Graph
import System.Directory.Tree
import ExceptionHandling
import Control.Monad.IO.Class

data ParsedModule a = ParsedModule {moduleName :: String, imports:: [String], moduleValue :: a } deriving (Show, Eq)

-- | Représentation de toutes les modules d'un programme
newtype Module a = Module [(a, String, [String])] deriving(Show, Functor, Eq)

type ExceptModule m a = ExceptT [ModuleError] m a

data ModuleError = 
      ImportNonExisting String
    | ImportTwice String
    | CircularDependency [String]
    | FileReadingError String String 
      deriving(Show)
    
-- | Vérifie si un module a été importé plus d'une fois
allDifferent :: Monad m => [String] -> ExceptModule m ()
allDifferent lst = throwIfNotEmpty (errors $ sort lst)
    where
        errors xs = map (ImportTwice . fst) $ filter (\(a,b) -> a == b) $ zip xs (drop 1 xs)
    
-- | Gestion des erreurs
ensureModuleValid :: Monad m => Set.Set String -> ParsedModule a -> ExceptModule m ()
ensureModuleValid indexes m = do 
    allDifferent $ imports m
    throwIfNotEmpty $ map ImportNonExisting $ filter (\n -> Set.notMember n indexes) (imports m)
    
-- | Empêche les dépendances circulaires
ensureGraphNodeValid :: SCC (a, String, [String]) -> Maybe ModuleError
ensureGraphNodeValid (CyclicSCC xs) = Just $ CircularDependency $ fmap (\(_, key, _) -> key) xs
ensureGraphNodeValid _ = Nothing

-- | Map une liste, mais en gardant un état interne (comme le fold). Monadique
mapAccumRM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m [c]
mapAccumRM _ _ [] = return []
mapAccumRM f st (x:xs) = do
    (st', x') <- f st x
    xs' <- mapAccumRM f st' xs
    return (x':xs')

-- | Map un module dans l'ordre des dépendances. La fonction de projection reçoit le résultats des modules parents. Comme s est un monoid, les résultats sont fusionnés quand un module dépends de plusieurs autres modules    
mapModuleM :: (Monoid s, Monad m) => (s -> a -> m s) -> Module a -> m (Module s)
mapModuleM f (Module ms) = Module <$> mapAccumRM insertResults mempty ms
    where 
        result k v env = f (fold $ Map.lookup k env) v 
        insertResults env (v, k, xs) = do
            r <- result k v env
            return (foldr (\n e -> Map.insertWith (<>) n r e) env xs, (r, k, xs))
    
invertDependency :: [ParsedModule a] -> [(a, String, [String])]
invertDependency xs = fmap (\(ParsedModule name _ value) -> (value, name, getChilds name)) xs
    where
        toNode (ParsedModule name' imp _) c = foldr (\n -> Map.insertWith (++) n [name']) c imp
        childs= foldr toNode mempty xs
        getChilds n = fromMaybe [] $ Map.lookup n childs
       
createValidGraph :: Monad m => [ParsedModule a] -> ExceptModule m (Module a) 
createValidGraph xs = do
        mapM_ (ensureModuleValid moduleSet) xs
        throwIfNotEmpty circulars
        return $ Module $ fmap getElem $ topSort graph
    where 
        circulars = mapMaybe ensureGraphNodeValid $ stronglyConnCompR elems
        (graph, getElem) = graphFromEdges' elems
        elems = invertDependency xs
        moduleSet = Set.fromList $ fmap moduleName xs
        
-- | Transforme la liste des dépendances en un seul module
joinModules (Module xs) = mconcat $ fmap (\(a,_,_) -> a) xs
       
       
getAllBaetaFiles :: FilePath -> ExceptModule IO [(FilePath, String)]
getAllBaetaFiles directory = do
    tree <- liftIO $ readDirectory directory
    mergeEithers $ flat directory (dirTree tree)
    where 
        flat dir (File n c) = [Right (n, c)]
        flat dir (Failed n e) = [Left (FileReadingError n (show e))]
        flat dir (Dir n c) = concatMap (flat n) c