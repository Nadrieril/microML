{-# LANGUAGE RecursiveDo #-}
module Eval where
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State

import IR.AST

data Val =
    VInt Integer
  | VBool Bool
  | VFun Env Name (Expr Name)
  | VSysCall (Val -> State Env Val)

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

evalAp :: Val -> Val -> State Env Val
evalAp (VFun env x v) y = do
    put (M.insert x y env)
    evalE v
evalAp (VSysCall f) y = f y
evalAp v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)


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
      f (op, fop) = (Name op, VSysCall $ \v1 -> (return $ VSysCall $ \v2 -> return $ fop v1 v2))


evalE :: Expr Name -> State Env Val
evalE (Var x) = (M.! x) <$> get
evalE (Const (B b)) = return $ VBool b
evalE (Const (I i)) = return $ VInt i
evalE (Infix o x y) = evalE $ Ap (Ap (Var o) x) y
evalE (Let x v e) = do
    vv <- evalE v
    modify (M.insert x vv)
    evalE e
evalE (LetR f v e) = do
    rec body <- do
        modify (M.insert f body)
        evalE v
    modify (M.insert f body)
    evalE e
evalE (If b e1 e2) = do
    vb <- evalE b
    case vb of
        VBool True -> evalE e1
        VBool False -> evalE e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE (Fun x e) = do
    env <- get
    return $ VFun env x e
evalE (Ap f x) = do
    vf <- evalE f
    vx <- evalE x
    evalAp vf vx


eval :: Expr Name -> Val
eval e = evalState (evalE e) stdLib
