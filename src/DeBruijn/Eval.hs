{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module DeBruijn.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Class (ask)
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (StateT, evalStateT, get, put)

import Utils (push, local)
import qualified StdLib (stdLib)
import DeBruijn.Expr (Identity(..), Expr, AbsExpr(..), Value(..), Name(..))


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

type Eval e = StateT (Env e) (Reader (M.Map Name (Val Expr))) (Val e)
getStack = get
getGlobals = ask

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
        evalAE e
evalAp (VSysCall f) y = f y
evalAp v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)

evalAE :: Expr -> Eval Expr
evalAE = evalE . runIdentity

evalE :: AbsExpr Identity -> Eval Expr
evalE (Var i) = (!! i) <$> getStack
evalE (Global g) = (M.! g) <$> getGlobals
evalE (Const (B b)) = return $ VBool b
evalE (Const (I i)) = return $ VInt i
evalE (If b e1 e2) = do
    vb <- evalAE b
    case vb of
        VBool True -> evalAE e1
        VBool False -> evalAE e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE (Fun _ e) = do
    stk <- get
    return $ VFun stk e
evalE (Fix _ e) =
    local $ do
        rec
            push body
            body <- evalAE e
        return body
evalE (Let _ v e) = do
    vv <- evalAE v
    local $ do
        push vv
        evalAE e
evalE (Ap f x) = do
    vf <- evalAE f
    vx <- evalAE x
    evalAp vf vx

eval :: Expr -> Val Expr
eval e = runReader (evalStateT (evalAE e) []) globals
