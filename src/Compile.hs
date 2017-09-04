-- | Simple petit module qui fait toute les étapes de la compilation
module Compile where
import Parser
import Closure
import TypeInference
import ModuleSystem
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Except
import ExceptionHandling
import Interpreter
import Data.Functor.Identity
import Text.PrettyPrint.ANSI.Leijen


fmap2 f = fmap (fmap f)
fmap3 f = fmap (fmap2 f)


getProgramAST :: Monad m => [(String, String)] -> WriterT String m (Either String [Named Function])
getProgramAST = fmap2 joinModules . runExceptT . parsingStep

getProgramTypes ::
  Monad m =>
  [(String, String)] -> WriterT String m (Either String Doc)
getProgramTypes files = fmap2 (pretty . joinModules) $ runExceptT $ do 
    m <- parsingStep files
    inferenceStep m

getProgramClosures ::
  Monad m =>
  [(String, String)]
  -> WriterT
       String m (Either String [Named (Expr (Environment Monotype))])
getProgramClosures files =  runExceptT $ do
    m <- parsingStep files
    i <- inferenceStep m
    return $ joinModules $ fmap3 (fmap snd . addClosure) i
    
getProgramOutput ::Monad m => [(String, String)] -> WriterT String m (Either String Doc)
getProgramOutput = fmap2 pretty . compile
    
parsingStep :: Monad m => [(String, String)] -> ExceptT String m (Module [Named Function])
parsingStep files = do
    parseResult <- mapM (\(name, content) -> mapError $ parseModule content name) files
    mapError $ createValidGraph parseResult
 
inferenceStep
  :: Monad m => Module [Named Function]
     -> ExceptT String (WriterT String m) (Module [Named (Expr Monotype)])
inferenceStep m = evalStateT (inferModule m) baseVariable

-- |Représente le flow de compilateur, du fichier texte au résultat final
compile :: Monad m => [(String, String)] -> WriterT String m (Either String ExprEnv)
compile files = runExceptT $ do 
    m <- parsingStep files
    i <- inferenceStep m
    evalProgram $ joinModules $ fmap3 addClosure i