{-# LANGUAGE FlexibleContexts, UndecidableInstances, RankNTypes #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (foldrM)
import Data.List (intercalate)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Text.Printf (printf)

import AST.Parse (isOperator)
import Common.Expr
import Common.ADT
import qualified Common.Context as C
import Common.StdLib (globalContext)
import DBT.Expr


type Env = [Val]

data Val =
    Val Value
  | VFun Env TypedExpr
  | VRecFun Env TypedExpr
  | VSysCall (Val -> Val)
  | VConstructor (ADT Id) Int Int [Val]
  | VDeconstructor (ADT Id) Int [Val]

instance PrettyPrint Val where
    pprint (Val x) = printf "Val %s" (pprint x)
    pprint (VFun _ e) = printf "VFun(\\%s)" (pprint e)
    pprint (VRecFun _ e) = printf "VRecFun(\\%s)" (pprint e)
    pprint VSysCall{} = printf "VSysCall"
    pprint (VConstructor adt n _ l) = let name = constructorName (adtConstructors adt !! n) in
        case reverse l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (pprint x) (pprint name) (pprint y)
            l -> printf "%s[%s]" (pprint name) (intercalate ", " (map pprint l))
    pprint (VDeconstructor adt _ _) = printf "%s.." (pprint $ deconstructorName adt)

instance Show Val where
    show =let ?toplevel = False in pprint

fromContext :: C.ContextValue -> Val
fromContext = \case
    C.Value v -> Val v
    C.Constructor adt n i -> VConstructor adt n i []
    C.Deconstructor adt i -> VDeconstructor adt i []
    C.SysCall f -> VSysCall (\(Val x) -> fromContext (f x))

getFree :: C.Context -> Name -> Val
getFree ctx x | Just (cv, _) <- x `M.lookup` ctx = fromContext cv
getFree _ x = error $ printf "Unknown free variable: %s" (show x)


type Eval r a = (
    Member (Reader C.Context) r,
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
evalE (expr -> Free g) = do
    ctx <- ask
    return $ getFree ctx g
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
evalE (expr -> SFix _) = error "Cannot evaluate arbitrary recursive expression"
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


eval :: Program -> Val
eval (ctx, e) = run $
    flip runReader (globalContext <> ctx) $
    evalState ([] :: [Val]) $
        evalE e
