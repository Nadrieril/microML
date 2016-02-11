{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module DeBruijn.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (State, evalState, get, put)

import Utils (push, local)
import qualified Common.StdLib as StdLib (stdLib)
import Common.Expr
import DeBruijn.Expr


type Env e = [Val e]

data Val e =
    VInt Integer
  | VBool Bool
  | VFun (Env e) e
  | VSysCall (Val e -> Eval e)

instance Show e => Show (Val e) where
  show (VInt i) = printf "VInt %s" (show i)
  show (VBool b) = printf "VBool %s" (show b)
  show (VFun _ e) = printf "VFun(\\%s)" (show e)
  show VSysCall{} = printf "VSysCall"

type Eval e = State (Env e) (Val e)

globals :: M.Map Name (Val Expr)
globals = fmap f StdLib.stdLib
    where
        tr (VInt i) = I i
        tr (VBool b) = B b
        tr v = error $ printf "Error: attempting to evaluate %s as value" (show v)
        tr' (I i) = VInt i
        tr' (B b) = VBool b
        f fop = let fop' v1 v2 = tr' (fop (tr v1) (tr v2)) in
            VSysCall $ \v1 -> (return $ VSysCall $ \v2 -> return $ fop' v1 v2)


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
evalE (expr -> Global g) = return $ globals M.! g
evalE (expr -> Const (B b)) = return $ VBool b
evalE (expr -> Const (I i)) = return $ VInt i
evalE (expr -> If b e1 e2) = do
    vb <- evalE b
    case vb of
        VBool True -> evalE e1
        VBool False -> evalE e2
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
