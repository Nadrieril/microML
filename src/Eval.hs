module Eval where
import qualified Data.Map as M
import IR.AST

evalOp :: BinOp -> Integer -> Integer -> Integer
evalOp Multiply = (*)
evalOp Divide = div
evalOp Add = (+)
evalOp Subtract = (-)
evalOp Or = (+)
evalOp And = (*)

type Env = M.Map String Integer

eval_ :: Env -> Expr -> Integer
eval_ env (Var x) = env M.! x
eval_ _ (BoolConst b) = if b then 1 else 0
eval_ _ (IntConst i) = i
eval_ env (Neg e) = - (eval_ env e)
eval_ env (ABinary o x y) = evalOp o (eval_ env x) (eval_ env y)
eval_ env (Let x v e) = eval_ (M.insert x (eval_ env v) env) e
eval_ _ _ = undefined

eval :: Expr -> Integer
eval = eval_ M.empty
