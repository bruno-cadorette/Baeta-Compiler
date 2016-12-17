-- | Ce module devait être utilisé pour une meilleure gestion des erreurs que seulement un message d'erreur mais a été un peu abandonné. Certaines fonctions sont tout de même utile

module ExceptionHandling where
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.Trans.Except
import Control.Monad.Logger
import Data.Either

newtype UserException = UserException Doc deriving (Show)

class Pretty a => Throws a where
    getException :: a -> UserException
    getException a = UserException (pretty a) 

-- | Une fonction peut quelques fois avoir plusieurs erreurs à la fois. Comme l'opérateur de bind du monad ExceptT continue seulement si il n'a pas d'erreur, nous voulons throw toutes les erreurs en même temps, sinon les autres opérations ne se feront pas après.
throwIfNotEmpty :: Monad m => [a] -> ExceptT [a] m () 
throwIfNotEmpty [] = return ()
throwIfNotEmpty xs = throwE xs

-- | Si plusieurs opérations ont été fait, nous les combinons ensemble afin de voir si il y a des erreurs
mergeEithers :: Monad m => [Either a b] -> ExceptT [a] m [b]
mergeEithers xs = do
        throwIfNotEmpty l
        return r
    where (l, r) = partitionEithers xs


-- | Il est impossible de combiner deux ExceptT si ils n'ont pas le même type d'erreur. Cette fonction s'assure de transformer l'erreur en String
mapError :: (Show e, Monad m) => ExceptT e m a -> ExceptT String m a 
mapError = withExceptT (show)