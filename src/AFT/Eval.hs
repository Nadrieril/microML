{-# LANGUAGE RecursiveDo #-}
module AFT.Eval
    ( eval
    ) where

import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (get, put, modify, evalState)

import AFT.Expr (Expr(..), Name(..), Value(..))
import AST.Eval (Eval, Val(..), stdLib)


evalAp :: Val Expr -> Val Expr -> Eval Expr
evalAp (VFun env x v) y = do
    put (M.insert x y env)
    evalE v
evalAp (VSysCall f) y = f y
evalAp v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)


evalE :: Expr Name -> Eval Expr
evalE (Var x) = (M.! x) <$> get
evalE (Const (B b)) = return $ VBool b
evalE (Const (I i)) = return $ VInt i
evalE (If b e1 e2) = do
    vb <- evalE b
    case vb of
        VBool True -> evalE e1
        VBool False -> evalE e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE (Fun x e) = do
    env <- get
    return $ VFun env x e
evalE (Fix f e) = do
    rec
        modify (M.insert f body)
        body <- evalE e
    return body
evalE (Ap f x) = do
    vf <- evalE f
    vx <- evalE x
    evalAp vf vx


eval :: Expr Name -> Val Expr
eval e = evalState (evalE e) stdLib
