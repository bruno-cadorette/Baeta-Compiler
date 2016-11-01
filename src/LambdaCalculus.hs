module LambdaCalculus where
import Text.PrettyPrint.ANSI.Leijen

data Expr = 
      Var VariableName
    | Apply Expr Expr
    | Lambda VariableName Expr
    deriving (Show, Eq, Ord)
    
type VariableName = String
instance Pretty Expr where
    pretty (Var var) = text var
    pretty (Apply expr1 expr2) = pretty expr1 <+> pretty expr2
    pretty (Lambda var expr) = char '\\' <+> text var <+> text "->" </> pretty expr
    --could be nice to print the greek letter