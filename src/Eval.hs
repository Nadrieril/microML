module Eval where
import qualified Data.Map as M
import Text.Printf (printf)

import IR.AST

type Var = String
data Val = VInt Integer | VBool Bool | VFun Var Expr
    deriving (Show)


withInt :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
withInt f (VInt x) (VInt y) = VInt $ f x y
withInt _ v _ = error $ printf "Error: attempting to evaluate %s as int" (show v)

withBool :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
withBool f (VBool x) (VBool y) = VBool $ f x y
withBool _ v _ = error $ printf "Error: attempting to evaluate %s as bool" (show v)

evalOp :: BinOp -> Val -> Val -> Val
evalOp Multiply = withInt (*)
evalOp Divide = withInt div
evalOp Add = withInt (+)
evalOp Subtract = withInt (-)
evalOp Or = withBool (||)
evalOp And = withBool (&&)

type Env = M.Map String Val

eval_ :: Env -> Expr -> Val
eval_ env (Var x) = env M.! x
eval_ _ (BoolConst b) = VBool b
eval_ _ (IntConst i) = VInt i
eval_ env (Neg e) = eval_ env (ABinary Subtract (IntConst 0) e)
eval_ env (ABinary o x y) = evalOp o (eval_ env x) (eval_ env y)
eval_ env (Let x v e) = eval_ (M.insert x (eval_ env v) env) e
eval_ _ _ = undefined

eval :: Expr -> Val
eval = eval_ M.empty
