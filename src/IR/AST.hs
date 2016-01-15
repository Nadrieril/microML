module IR.AST where
import Text.Printf (printf)

data BinOp = Multiply | Divide | Add | Subtract | Or | And

instance Show BinOp where
  show Multiply = "*"
  show Divide = "/"
  show Add = "+"
  show Subtract = "-"
  show Or = "or"
  show And = "and"


data Expr =
      Var String
    | BoolConst Bool
    | IntConst Integer
    | Neg Expr
    | ABinary BinOp Expr Expr
    | Ap Expr Expr
    | Let String Expr Expr

instance Show Expr where
  show (Var x) = x
  show (BoolConst b) = if b then "True" else "False"
  show (IntConst i) = show i
  show (Neg e) = "-" ++ show e
  show (ABinary o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
  show (Ap f x) = printf "(%s %s)" (show f) (show x)
  show (Let x v e) = printf "let %s = %s in %s" (show x) (show v) (show e)
