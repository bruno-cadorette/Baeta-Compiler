module ModuleSystem (Module(..), createValidGraph, ModuleError(..), ModuleProgram, mapModuleM) where
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Graph

data Module k a = Module {moduleName :: k, imports:: [k], moduleValue :: a } deriving (Show)

newtype ModuleProgram a = ModuleProgram [(a, String, [String])] deriving(Show)

data ModuleError = 
    ImportNonExisting String |
    ImportTwice String |
    CircularDependency [String] deriving(Show)
    

throwIfNotEmpty :: [a] -> Except [a] () 
throwIfNotEmpty [] = return ()
throwIfNotEmpty xs = throwE xs
    
allDifferent :: [String] -> Except [ModuleError] ()
allDifferent lst = throwIfNotEmpty (errors $ sort lst)
    where
        errors xs = map (ImportTwice . fst) $ filter (\(a,b) -> a == b) $ zip xs (drop 1 xs)
    
ensureModuleValid :: Set.Set String -> Module String a -> Except [ModuleError] ()
ensureModuleValid indexes m = do 
    allDifferent $ imports m
    throwIfNotEmpty $ map ImportNonExisting $ filter (\n -> Set.notMember n indexes) (imports m)
    
ensureGraphNodeValid :: SCC (a, String, [String]) -> Maybe ModuleError
ensureGraphNodeValid (CyclicSCC xs) = Just $ CircularDependency $ fmap (\(_, key, _) -> key) xs
ensureGraphNodeValid _ = Nothing

mapAccumRM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m [c]
mapAccumRM _ _ [] = return []
mapAccumRM f st (x:xs) = do
    (st', x') <- f st x
    xs' <- mapAccumRM f st' xs
    return (x':xs')

mapModuleM :: (Monoid s, Monad m) => (s -> a -> m s) -> ModuleProgram a -> m (ModuleProgram s)
mapModuleM f (ModuleProgram ms) = ModuleProgram <$> mapAccumRM insertResults mempty ms
    where 
        result k v env = f (env Map.! k) v 
        insertResults env (v, k, xs) = do
            r <- result k v env
            return (foldr (\n e -> Map.insertWith (<>) n r e) env xs, (r, k, xs))
    
invertDependency :: [Module String a] -> [(a, String, [String])]
invertDependency xs = fmap (\(Module name _ value) -> (value, name, getChilds name)) xs
    where
        toNode (Module name imp _) c = foldr (\n -> Map.insertWith (++) n [name]) c imp
        childs= foldr toNode mempty xs
        getChilds n = fromMaybe [] $ Map.lookup n childs
       
createValidGraph :: [Module String a] -> Except [ModuleError] (ModuleProgram a) 
createValidGraph xs = do
        mapM_ (ensureModuleValid moduleSet) xs
        throwIfNotEmpty circulars
        return $ ModuleProgram $ fmap getElem $ topSort graph
    where 
        circulars = mapMaybe ensureGraphNodeValid $ stronglyConnCompR elems
        (graph, getElem) = graphFromEdges' elems
        elems = invertDependency xs
        moduleSet = Set.fromList $ fmap moduleName xs