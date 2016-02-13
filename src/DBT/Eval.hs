{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad (foldM)
import Control.Monad.State (State, evalState, get, put)
import qualified Debug.Trace as T

import Utils (push, local)
import AST.Parse (isOperator)
import qualified Common.StdLib as StdLib
import Common.Expr
import Common.ADT
import DBT.Expr


type Env e = [Val e]

data Val e =
    Val Value
  | VProduct Name [Val e]
  | VFun (Env e) e
  | VSysCall (Val e -> Eval e)
  | VConstructor Name Int [Val e]
  | VDeconstructor (ADT Name) Int [Val e]

instance Show e => Show (Val e) where
  show (Val x) = printf "Val %s" (show x)
  show (VProduct n [x, y]) | isOperator n = printf "(%s %s %s)" (show x) (show n) (show y)
  show (VProduct n l) = printf "%s%s" (show n) (show l)
  show (VFun _ e) = printf "VFun(\\%s)" (show e)
  show VSysCall{} = printf "VSysCall"
  show VConstructor{} = printf "VConstructor"
  show VDeconstructor{} = printf "VDeconstructor"

type Eval e = State (Env e) (Val e)


evalSysCall :: Name -> Val Expr
evalSysCall x | x `M.member` adtMap =
        let adt = fst $ adtMap M.! x in
        if x == deconstructorName adt
            then VDeconstructor adt (length $ adtConstructors adt) []
            else let Constructor n p = constructorsMap adt M.! x in
                if null p
                    then VProduct n []
                    else VConstructor n (length p) []

evalSysCall x = aux $ StdLib.sysCallToValue $ StdLib.getSysCall x
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
evalAp (VConstructor n i l) x = return $
    if i == 1
        then VProduct n (reverse $ x:l)
        else VConstructor n (i-1) (x:l)
evalAp (VDeconstructor adt i p) x =
    if i == 0
        then do
            let VProduct n l = x
            let f = reverse p !! fromJust (n `elemIndex` map constructorName (adtConstructors adt))
            foldM evalAp f l
        else return $ VDeconstructor adt (i-1) (x:p)
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
