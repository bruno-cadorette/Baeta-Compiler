module TypeInference where
import TypeInference.Base
import TypeInference.Rules
import TypeInference.TypeVariable
import Control.Monad.Trans.Except
import Control.Monad.State.Lazy
import Data.Functor.Identity
import LambdaCalculus
import Parser


runInference e expr =  runIdentity $ evalStateT (runExceptT (infer e expr) ) baseVariable
    

identityFunc = Lambda "x" (Var "y")