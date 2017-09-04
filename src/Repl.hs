module Repl() where
import Parser
import TypeInference
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Text.PrettyPrint.ANSI.Leijen

readInput env input = do
    func <-lift $ parseFunction input
    inferFunction env func

repl :: MonadIO m => TypeEnvironment -> [Named TypedExpr] -> InferenceMonad m ()
repl env xs = do
    input <- liftIO $ getLine
    (env', x) <- readInput env input
    liftIO $ print $ pretty x
    repl env' (x:xs)