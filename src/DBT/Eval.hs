{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import Text.Printf (printf)
import Control.Monad.State (State, evalState, get, put)

import Utils (push, local)
import qualified Common.StdLib as StdLib
import Common.Expr
import DBT.Expr


type Env e = [Val e]

data Val e =
    Val Value
  | VFun (Env e) e
  | VSysCall (Val e -> Eval e)

instance Show e => Show (Val e) where
  show (Val x) = printf "Val %s" (show x)
  show (VFun _ e) = printf "VFun(\\%s)" (show e)
  show VSysCall{} = printf "VSysCall"

type Eval e = State (Env e) (Val e)


evalSysCall :: Name -> Val Expr
evalSysCall sc = aux $ StdLib.sysCallToValue $ StdLib.getSysCall sc
    where
        aux (StdLib.Val v) = Val v
        aux (StdLib.Fun f) = VSysCall (\(Val x) -> return $ aux (f x))

evalAp :: Val Expr -> Val Expr -> Eval Expr
evalAp (VFun stk e) y =
    local $ do
        put stk
        push y
        evalE e
evalAp (VSysCall f) y = f y
evalAp v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)

evalE :: Expr -> Eval Expr
evalE (expr -> Var i) = (!! i) <$> get
evalE (expr -> Global g) = return $ evalSysCall g
evalE (expr -> Const x) = return $ Val x
evalE (expr -> If b e1 e2) = do
    vb <- evalE b
    case vb of
        Val (B True) -> evalE e1
        Val (B False) -> evalE e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE (expr -> Fun _ e) = do
    stk <- get
    return $ VFun stk e
evalE (expr -> Fix _ e) =
    local $ do
        rec
            push body
            body <- evalE e
        return body
evalE (expr -> Let _ v e) = do
    vv <- evalE v
    local $ do
        push vv
        evalE e
evalE (expr -> Ap f x) = do
    vf <- evalE f
    vx <- evalE x
    evalAp vf vx
evalE _ = error "impossible"

eval :: Expr -> Val Expr
eval e = evalState (evalE e) []
