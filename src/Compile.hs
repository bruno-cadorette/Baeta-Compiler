-- | Simple petit module qui fait toute les étapes de la compilation
module Compile(compile) where
import Parser
import Closure
import TypeInference
import ModuleSystem
import Control.Monad.Trans.Except
import ExceptionHandling
import Interpreter


fmap2 f = fmap (fmap f)
fmap3 f = fmap (fmap2 f)

-- |Représente le flow de compilateur, du fichier texte au résultat final
compile :: Monad m => [(String, String)] -> m (Either String ExprEnv)
compile files = runExceptT $ do 
    parseResult <- mapM (\(name, content) -> mapError $ parseModule content name) files
    m <- mapError $ createValidGraph parseResult
    i <- runInference $ inferModule m
    evalProgram $ joinModules $ fmap3 addClosure i
    