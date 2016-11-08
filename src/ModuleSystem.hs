{-# LANGUAGE DeriveFunctor #-}
module ModuleSystem (Module(..), createValidGraph, ModuleError(..), ModuleProgram, mapModuleM) where
import Control.Monad
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Graph
import Data.Either
import qualified Data.MultiMap as Multimap

data Module a k = Module {moduleName :: k, imports:: [k], moduleValue :: a } deriving (Functor, Show)

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
    
ensureModuleValid :: Set.Set String -> Module a String -> Except [ModuleError] ()
ensureModuleValid indexes (Module k ns v) = do 
    allDifferent ns
    throwIfNotEmpty $ map ImportNonExisting $ filter (\n -> Set.notMember n indexes) ns
    
ensureGraphNodeValid :: SCC (a, String, [String]) -> Either ModuleError (a, String, [String])
ensureGraphNodeValid (AcyclicSCC a) = Right a
ensureGraphNodeValid (CyclicSCC xs) = Left $ CircularDependency $ fmap (\(_, key, _) -> key) xs

mapAccumRM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m [c]
mapAccumRM _ _ [] = return []
mapAccumRM f st (x:xs) = do
    (st', x') <- f st x
    xs' <- mapAccumRM f st' xs
    return (x':xs')

mapModuleM :: (Monoid s, Monad m) => (s -> a -> m s) -> ModuleProgram a -> m (ModuleProgram s)
mapModuleM f (ModuleProgram xs) = ModuleProgram <$> mapAccumRM insertResults mempty xs
    where 
        result k v env = f (env Map.! k) v 
        insertResults env (v, k, xs) = do
            r <- result k v env
            return (foldr (\n e -> Map.insertWith (<>) n r e) env xs, (r, k, xs))
            
--Donne seulement le path
toGraph xs = do
        throwIfNotEmpty circulars
        return $ ModuleProgram validNodes
    where 
        (circulars, validNodes) = partitionEithers $ fmap ensureGraphNodeValid $ stronglyConnCompR elems
        elems = Map.elems $ Map.intersectionWithKey (\k v c -> (v, k, c)) values (Multimap.toMap childs) 
        (values, childs) = foldr toNode (mempty, Multimap.empty) xs
        toNode (Module name imp value) (v,c) = 
            (Map.insert name value v, foldr (\n -> Multimap.insert n name) c imp)
        
createValidGraph :: [Module a String] -> Except [ModuleError] (ModuleProgram a)
createValidGraph xs = do
    mapM_ (ensureModuleValid moduleSet) xs
    toGraph xs
    where
        moduleSet = Set.fromList $ fmap moduleName xs
        
    
