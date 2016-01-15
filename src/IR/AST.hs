module IR.AST where
import Text.Printf (printf)

data BinOp = Multiply | Divide | Add | Subtract | Or | And | Eq

instance Show BinOp where
  show Multiply = "*"
  show Divide = "/"
  show Add = "+"
  show Subtract = "-"
  show Or = "or"
  show And = "and"
  show Eq = "=="


data Expr =
      Var String
    | BoolConst Bool
    | IntConst Integer
    | Neg Expr
    | ABinary BinOp Expr Expr
    | Ap Expr Expr
    | Let String Expr Expr
    | If Expr Expr Expr
    | Fun String Expr

instance Show Expr where
  show (Var x) = x
  show (BoolConst b) = if b then "True" else "False"
  show (IntConst i) = show i
  show (Neg e) = "-" ++ show e
  show (ABinary o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
  show (Ap f x) = printf "(%s %s)" (show f) (show x)
  show (Let x v e) = printf "let %s = %s in\n%s" x (show v) (show e)
  show (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
  show (Fun x e) = printf "(fun %s -> %s)" (show x) (show e)
