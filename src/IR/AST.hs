module IR.AST where

data BinOp = Multiply | Divide | Add | Subtract | Or | And
  deriving (Show)

data Expr =
      Var String
    | IntConst Integer
    | Neg Expr
    | ABinary BinOp Expr Expr
    | Ap Expr Expr
    deriving (Show)
