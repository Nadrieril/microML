module Eval where
import qualified Data.Map as M
import Text.Printf (printf)

import IR.AST

data Val =
    VInt Integer
  | VBool Bool
  | VFun Env Name (Expr Name)
  | VSysCall (Val -> Val)

instance Show Val where
  show (VInt i) = printf "VInt %s" (show i)
  show (VBool b) = printf "VBool %s" (show b)
  show VFun{} = printf "VFun"
  show VSysCall{} = printf "VSysCall"


withInt :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
withInt f (VInt x) (VInt y) = VInt $ f x y
withInt _ v _ = error $ printf "Error: attempting to evaluate %s as int" (show v)

withBool :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
withBool f (VBool x) (VBool y) = VBool $ f x y
withBool _ v _ = error $ printf "Error: attempting to evaluate %s as bool" (show v)

ap :: Val -> Val -> Val
ap (VFun env x v) y = evalE (M.insert x y env) v
ap (VSysCall f) y = f y
ap v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)


type Env = M.Map Name Val

stdLib :: Env
stdLib = M.fromList $ map f [
      ("*", withInt (*)),
      ("/", withInt div),
      ("+", withInt (+)),
      ("-", withInt (-)),
      ("or", withBool (||)),
      ("and", withBool (&&)),
      ("==", eq)
    ]
    where
      eq (VInt x) (VInt y) = VBool (x==y)
      eq (VBool x) (VBool y) = VBool (x==y)
      eq x y = error $ printf "Error: %s and %s cannot be compared" (show x) (show y)
      f (op, fop) = (Name op, VSysCall $ \v1 -> (VSysCall $ \v2 -> fop v1 v2))


evalE :: Env -> Expr Name -> Val
evalE env (Var x) = env M.! x
evalE _ (Const (B b)) = VBool b
evalE _ (Const (I i)) = VInt i
evalE env (Infix o x y) = evalE env $ Ap (Ap (Var o) x) y

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
eval = evalE stdLib
