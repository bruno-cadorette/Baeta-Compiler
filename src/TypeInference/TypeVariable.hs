{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- | S'occupe du nommage des variables de type
module TypeInference.TypeVariable (TypeVariable(..), newTypeVariable, baseVariable) where
import Control.Monad.State.Class
import Text.PrettyPrint.ANSI.Leijen

-- | Variable de type
newtype TypeVariable =  TypeVariable Int
    deriving (Show, Eq, Ord, Enum)
    
instance Pretty TypeVariable where
    pretty (TypeVariable v) = text $ show v

-- | Instancie une nouvelle variable avec un nom unique
newTypeVariable :: MonadState TypeVariable m => m TypeVariable
newTypeVariable = do
    v <- get
    put (succ v :: TypeVariable)
    return v
    
baseVariable :: TypeVariable
baseVariable = TypeVariable 0