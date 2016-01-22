module Eval where
import qualified Data.Map as M
import Text.Printf (printf)

import IR.AST

type Var = String
data Val = VInt Integer | VBool Bool | VFun Env Var (Expr Name)
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
evalOp Eq = f
    where
      f (VInt x) (VInt y) = VBool (x==y)
      f (VBool x) (VBool y) = VBool (x==y)
      f x y = error $ printf "Error: %s and %s cannot be compared" (show x) (show y)

ap :: Val -> Val -> Val
ap (VFun env x v) y = evalE (M.insert x y env) v
ap v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)


type Env = M.Map String Val

evalE :: Env -> Expr Name -> Val
evalE env (Var x) = env M.! x
evalE _ (BoolConst b) = VBool b
evalE _ (IntConst i) = VInt i
evalE env (Neg e) = evalE env (ABinary Subtract (IntConst 0) e)
evalE env (ABinary o x y) = evalOp o (evalE env x) (evalE env y)
evalE env (Let x v e) = evalE (M.insert x (evalE env v) env) e
evalE env (LetR f v e) = evalE (M.insert f body env) e
    where body = evalE (M.insert f body env) v
evalE env (If b e1 e2) =
    case evalE env b of
        VBool True -> evalE env e1
        VBool False -> evalE env e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE env (Fun x e) = VFun env x e
evalE env (Ap f x) = ap (evalE env f) (evalE env x)

eval :: Expr Name -> Val
eval = evalE M.empty
