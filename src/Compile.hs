-- | Simple petit module qui fait toute les étapes de la compilation
module Compile where
import Parser
import Closure
import TypeInference
import ModuleSystem
import Control.Monad.Trans.Except
import ExceptionHandling
import Interpreter
import Data.Functor.Identity
import Text.PrettyPrint.ANSI.Leijen


fmap2 f = fmap (fmap f)
fmap3 f = fmap (fmap2 f)



getProgramAST = fmap joinModules . runExcept . parsingStep

getProgramTypes files = fmap (pretty . joinModules) $ runExcept $ do 
    m <- parsingStep files
    inferenceStep m

getProgramClosures files =  runExcept $ do
    m <- parsingStep files
    i <- inferenceStep m
    return $ joinModules $ fmap3 (fmap snd . addClosure) i
    
getProgramOutput = fmap pretty . runIdentity . compile
    
parsingStep :: Monad m => [(String, String)] -> ExceptT String m (Module [Named Function])
parsingStep files = do
    parseResult <- mapM (\(name, content) -> mapError $ parseModule content name) files
    mapError $ createValidGraph parseResult
 
inferenceStep
  :: Monad m => Module [Named Function]
     -> ExceptT String m (Module [Named (Expr Monotype)])
inferenceStep = runInference . inferModule

-- |Représente le flow de compilateur, du fichier texte au résultat final
compile :: Monad m => [(String, String)] -> m (Either String ExprEnv)
compile files = runExceptT $ do 
    m <- parsingStep files
    i <- inferenceStep m
    evalProgram $ joinModules $ fmap3 addClosure i
    