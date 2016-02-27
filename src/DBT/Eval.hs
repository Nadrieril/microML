{-# LANGUAGE FlexibleContexts, UndecidableInstances, RankNTypes #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import qualified Data.Map as M
import Text.Printf (printf)
import Data.Foldable (foldrM)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)
import qualified Debug.Trace as T

import AST.Parse (isOperator)
import qualified Common.StdLib as StdLib
import Common.Expr
import Common.ADT
import qualified Common.Context as C
import DBT.Expr


type Env = [Val]

data Val =
    Val Value
  | VFun Env TypedExpr
  | VRecFun Env TypedExpr
  | VSysCall (Val -> Val)
  | VConstructor (ADT Id) Int Int [Val]
  | VDeconstructor (ADT Id) Int [Val]

instance Show Val where
    show (Val x) = printf "Val %s" (show x)
    show (VFun _ e) = printf "VFun(\\%s)" (show e)
    show (VRecFun _ e) = printf "VRecFun(\\%s)" (show e)
    show VSysCall{} = printf "VSysCall"
    show (VConstructor adt n _ l) = let name = constructorName (adtConstructors adt !! n) in
        case reverse l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (show x) (show name) (show y)
            l -> printf "%s%s" (show name) (show l)
    show (VDeconstructor adt _ _) = printf "%s.." (show $ deconstructorName adt)


getFree :: C.Context -> Name -> Val
getFree ctx x | Just (cv, _) <- x `M.lookup` ctx =
    case cv of
        C.Value v -> Val v
        C.Constructor adt n i -> VConstructor adt n i []
        C.Deconstructor adt i -> VDeconstructor adt i []
        C.SysCall f -> VSysCall (\(Val x) -> aux (f x))
        where
            aux (StdLib.Val v) = Val v
            aux (StdLib.Fun f) = VSysCall (\(Val x) -> aux (f x))
getFree _ x = error $ printf "Unknown free variable: %s" (show x)


type Eval r a = (
    Member (State Env) r)
    => Eff r a

push :: Val -> Eval r ()
push x = modify (x:)

local :: Eval r b -> Eval r b
local m = do
    (old :: Env) <- get
    ret <- m
    put old
    return ret


evalAp :: Val -> Val -> Eval r Val
evalAp (VFun stk e) y =
    local $ do
        put stk
        push y
        evalE e
evalAp f@(VRecFun stk e) y =
    local $ do
        put stk
        push f
        push y
        evalE e
evalAp (VSysCall f) y = return $ f y
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

evalE :: TypedExpr -> Eval r Val
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
evalE (expr -> SFix (expr -> SFun e)) = do
    stk <- get
    return $ VRecFun stk e
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


eval :: TypedExpr -> Val
eval e = run $
    evalState ([] :: [Val]) $
        evalE e
