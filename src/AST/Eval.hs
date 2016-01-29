{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module AST.Eval
    ( Env
    , Val(..)
    , Eval
    , stdLib
    , eval
    ) where

import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (State, get, put, modify, evalState)

import qualified StdLib (stdLib)
import Utils (local)
import AST.Expr (Expr(..), Name(..), Value(..))

type Env e = M.Map Name (Val e)

data Val e =
    VInt Integer
  | VBool Bool
  | VFun (Env e) Name (e Name)
  | VSysCall (Val e -> Eval e)

type Eval e = State (Env e) (Val e)

instance Show (e Name) => Show (Val e) where
  show (VInt i) = printf "VInt %s" (show i)
  show (VBool b) = printf "VBool %s" (show b)
  show (VFun _ n e) = printf "VFun(\\%s -> %s)" (show n) (show e)
  show VSysCall{} = printf "VSysCall"


stdLib :: Show (e Name) => Env e
stdLib = fmap f StdLib.stdLib
    where
        tr (VInt i) = I i
        tr (VBool b) = B b
        tr v = error $ printf "Error: attempting to evaluate %s as value" (show v)
        tr' (I i) = VInt i
        tr' (B b) = VBool b
        f fop = let fop' v1 v2 = tr' (fop (tr v1) (tr v2)) in
            VSysCall $ \v1 -> (return $ VSysCall $ \v2 -> return $ fop' v1 v2)


evalAp :: Val Expr -> Val Expr -> Eval Expr
evalAp (VFun env x v) y =
    local $ do
        put (M.insert x y env)
        evalE v
evalAp (VSysCall f) y = f y
evalAp v _ = error $ printf "Error: attempting to evaluate %s as function" (show v)


evalE :: Expr Name -> Eval Expr
evalE (Var x) = (M.! x) <$> get
evalE (Const (B b)) = return $ VBool b
evalE (Const (I i)) = return $ VInt i
evalE (Infix o x y) = evalE $ Ap (Ap (Var o) x) y
evalE (Let x v e) = do
    vv <- evalE v
    local $ do
        modify (M.insert x vv)
        evalE e
evalE (LetR f v e) =
    local $ do
        rec
            modify (M.insert f body)
            body <- evalE v
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


eval :: Expr Name -> Val Expr
eval e = evalState (evalE e) stdLib
