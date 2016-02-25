{-# LANGUAGE RecursiveDo, FlexibleContexts, UndecidableInstances #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import qualified Data.Map as M
import Text.Printf (printf)
import Data.Foldable (foldrM)
import Control.Monad.State (State, evalState, get, put)
import qualified Debug.Trace as T

import Utils (push, local)
import AST.Parse (isOperator)
import qualified Common.StdLib as StdLib
import Common.Expr
import Common.ADT hiding (Id)
import qualified Common.Context as C
import DBT.Expr


type Env e = [Val e]

data Val e =
    Val Value
  | VFun (Env e) e
  | VSysCall (Val e -> Eval e)
  | VConstructor (ADT Id) Int Int [Val e]
  | VDeconstructor (ADT Id) Int [Val e]

instance Show e => Show (Val e) where
    show (Val x) = printf "Val %s" (show x)
    show (VFun _ e) = printf "VFun(\\%s)" (show e)
    show VSysCall{} = printf "VSysCall"
    show (VConstructor adt n _ l) = let name = constructorName (adtConstructors adt !! n) in
        case reverse l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (show x) (show name) (show y)
            l -> printf "%s%s" (show name) (show l)
    show (VDeconstructor adt _ _) = printf "%s.." (show $ deconstructorName adt)


getFree :: C.Context -> Name -> Val Expr
getFree ctx x | Just (cv, _) <- x `M.lookup` ctx =
    case cv of
        C.Value v -> Val v
        C.Constructor adt n i -> VConstructor adt n i []
        C.Deconstructor adt i -> VDeconstructor adt i []
        C.SysCall f -> VSysCall (\(Val x) -> return $ aux (f x))
        where
            aux (StdLib.Val v) = Val v
            aux (StdLib.Fun f) = VSysCall (\(Val x) -> return $ aux (f x))
getFree _ x = error $ printf "Unknown free variable: %s" (show x)


type Eval e = State (Env e) (Val e)

evalAp :: Val Expr -> Val Expr -> Eval Expr
evalAp (VFun stk e) y =
    local $ do
        put stk
        push y
        evalE e
evalAp (VSysCall f) y = f y
evalAp (VConstructor _ _ 0 _) _ = error "Attempting to evaluate product as function"
evalAp (VConstructor adt n i l) x = return $ VConstructor adt n (i-1) (x:l)
evalAp (VDeconstructor (adtName -> n) 0 _) (VConstructor (adtName -> n') _ 0 _)
        | n /= n' = error $ printf "Attempting to deconstruct %s value as a %s" (show n) (show n')
evalAp (VDeconstructor _ 0 p) (VConstructor _ n 0 l) = do
        let f = p !! (length p - 1 - n)
        foldrM (flip evalAp) f l
evalAp (VDeconstructor _ 0 _) _ = error "Attempting to deconstruct non-product value"
evalAp (VDeconstructor adt i p) x = return $ VDeconstructor adt (i-1) (x:p)
evalAp v _ = error $ printf "Error: attempting to evaluate %s as a function" (show v)

evalE :: Expr -> Eval Expr
evalE (expr -> Bound i) = (!! i) <$> get
evalE (expr -> Free g) = return $ getFree C.globalContext g
evalE (expr -> Const x) = return $ Val x
evalE (expr -> If b e1 e2) = do
    vb <- evalE b
    case vb of
        Val (B True) -> evalE e1
        Val (B False) -> evalE e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalE (expr -> SFun e) = do
    stk <- get
    return $ VFun stk e
evalE (expr -> SFix e) =
    local $ do
        rec
            push body
            body <- evalE e
        return body
evalE (expr -> SLet v e) = do
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
