module ExceptionHandling where
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.Trans.Except
import Control.Monad.Logger
import Data.Either

newtype UserException = UserException Doc deriving (Show)

class Pretty a => Throws a where
    getException :: a -> UserException
    getException a = UserException (pretty a) 

throwIfNotEmpty :: Monad m => [a] -> ExceptT [a] m () 
throwIfNotEmpty [] = return ()
throwIfNotEmpty xs = throwE xs

mergeEithers :: Monad m => [Either a b] -> ExceptT [a] m [b]
mergeEithers xs = do
        throwIfNotEmpty l
        return r
    where (l, r) = partitionEithers xs
          

newtype CompileT m a = CompileT (ExceptT String m a)

--runCompileT (CompileT x) = runExceptT x


runCompileT m = runStdoutLoggingT . runExceptT

left a b = a
right a b = b

mapError :: (Show e, Monad m) => ExceptT e m a -> ExceptT String m a 
mapError = withExceptT (show)