{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module DBT.Eval
    ( Env
    , Val(..)
    , Eval
    , eval
    ) where

import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Foldable (asum)
import Data.List (intercalate)
import Control.Monad (forM)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Text.Printf (printf)

import Parse.Token (isOperator)
import Common.ADT
import Common.Expr
import Common.Pattern
import qualified Common.Context as C
import Common.StdLib (globalContext)
import Utils.PrettyPrint
import DBT.Expr


type Env = [Val]

data Val =
    Val Value
  | VFun Env TypedExpr
  | VRecFun Env TypedExpr
  | VSysCall (Val -> Val)
  | VConstructor (ADT Id) Int Int [Val]

data Deferred a = Deferred TypedExpr
    deriving (Functor, Foldable, Traversable, Show)

instance PrettyPrint Val where
    pprint (Val x) = printf "Val %s" (pprint x)
    pprint (VFun _ e) = printf "VFun(\\%s)" (pprint e)
    pprint (VRecFun _ e) = printf "VRecFun(\\%s)" (pprint e)
    pprint (VSysCall _) = printf "VSysCall"
    pprint (VConstructor adt n _ l) = let name = constructorName (adtConstructors adt !! n) in
        case reverse l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (pprint x) (pprint name) (pprint y)
            l -> printf "%s[%s]" (pprint name) (intercalate ", " (map pprint l))

instance Show Val where
    show = let ?toplevel = False in pprint

instance PrettyPrint (Deferred a) where
    pprint (Deferred x) = pprint x



fromContext :: C.ContextValue -> Val
fromContext = \case
    C.Value v -> Val v
    C.Constructor adt n i -> VConstructor adt n i []
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
pushAll :: [Val] -> Eval r ()
pushAll l = modify (l++)

local :: Eff r a -> Eval r a
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
evalAp v _ = error $ printf "Error: attempting to evaluate %s as a function" (show v)


evalVal :: AbstractExpr Deferred Val -> Eval r Val
evalVal (Bound _ i) = (!! i) <$> get
evalVal (Free g) = do
    ctx <- ask
    return $ getFree ctx g
evalVal (Const x) = return $ Val x
evalVal (If b e1 e2) =
    case b of
        Val (B True) -> return e1
        Val (B False) -> return e2
        v -> error $ printf "Error: attempting to evaluate %s as bool" (show v)
evalVal (Fun (Deferred s)) = do
    stk <- get
    return $ VFun stk s
evalVal (Fix (Deferred (deferScopes . expr -> Fun (Deferred s)))) = do
    stk <- get
    return $ VRecFun stk s
evalVal (Fix _) = error "Cannot evaluate arbitrary recursive expression"
evalVal (Let v (Deferred s)) = local $ do
        push v
        evalE s
evalVal (Ap f x) = evalAp f x
evalVal ex@(Match v l) = do
    (vals, Deferred s) <- case asum [ (, s) <$> matchVal v p | (p, s) <- l ] of
        Nothing -> error $ printf "Error: non-exhaustive pattern-matching in %s" (show ex)
        Just x -> return x
    local $ do
        pushAll vals
        evalE s
    where
        matchVal :: Val -> Pattern BoundVar -> Maybe [Val]
        matchVal (VConstructor (adtConstructors -> ctors) i 0 l) (Pattern n pats)
            | n == constructorName (ctors !! i) = do
                matches <- forM (zip (reverse l) pats) $ uncurry matchVal
                return $ concat matches
        matchVal x (PVar _) = Just [x]
        matchVal _ _ = Nothing


deferScopes :: AbstractExpr Scope TypedExpr -> AbstractExpr Deferred TypedExpr
deferScopes = scopeMap (\(Scope _ x) -> Deferred x)

evalE :: TypedExpr -> Eval r Val
evalE e = traverse evalE (deferScopes (expr e)) >>= evalVal


eval :: Program -> Val
eval (ctx, e) = run $
    flip runReader (globalContext <> ctx) $
    evalState ([] :: [Val]) $
        evalE e
