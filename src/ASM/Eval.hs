{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module ASM.Eval where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import qualified Debug.Trace as T
import Control.Monad (replicateM_, void, unless)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, evalState)
import qualified Control.Eff.State.Strict as State (get, put, modify)

import qualified Common.Expr as Expr (Value(..))
import ASM.Instr


get :: (Typeable a, Member (State a) r) => Proxy a -> Eff r a
get _ = State.get

put :: (Typeable a, Member (State a) r) => Proxy a -> a -> Eff r ()
put _ = State.put

modify :: (Typeable a, Member (State a) r) => Proxy a -> (a -> a) -> Eff r ()
modify _ = State.modify

type Stack a = [a]

class Stackable b a | a -> b where
    wrapStack :: Stack b -> a
    unwrapStack :: a -> Stack b

instance Stackable a (Stack a) where
    wrapStack = id
    unwrapStack = id


push :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> b -> Eff r ()
push p x = modify p (wrapStack . (x:) . unwrapStack)

popM :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> Eff r (Maybe b)
popM p = do
    xs <- get p
    case unwrapStack xs of
        [] -> return Nothing
        x:xs -> put p (wrapStack xs) >> return (Just x)

pop :: (Stackable b a, Typeable a, Member (State a) r) => Proxy a -> Eff r b
pop p = fromJust <$> popM p


data Value = Value Expr.Value | Closure (Code, Env)
    deriving (Show)

newtype Code = Code (Stack Instr)
    deriving (Show, Stackable Instr)
newtype Env = Env (Stack Value)
    deriving (Show, Stackable Value)
newtype ValStack = ValStack (Stack Value)
    deriving (Show, Stackable Value)
newtype CallStack = CallStack (Stack (Code, Env))
    deriving (Show, Stackable (Code, Env))


code :: Proxy Code
code = Proxy
env :: Proxy Env
env = Proxy
valstack :: Proxy ValStack
valstack = Proxy
callstack :: Proxy CallStack
callstack = Proxy



evalOp :: BinOp -> Expr.Value -> Expr.Value -> Expr.Value
evalOp o = case o of
    Plus -> withInt (+)
    Minus -> withInt (-)
    Mult -> withInt (*)
    Div -> withInt div
    And -> withBool (&&)
    Or -> withBool (||)
    Eq -> eq
    where
        withInt f (Expr.I x) (Expr.I y) = Expr.I $ f x y
        withInt _ _ _ = error "impossible"

        withBool f (Expr.B x) (Expr.B y) = Expr.B $ f x y
        withBool _ _ _ = error "impossible"

        eq (Expr.I x) (Expr.I y) = Expr.B (x==y)
        eq (Expr.B x) (Expr.B y) = Expr.B (x==y)
        eq _ _ = error "impossible"


type ASMEval r e =
    ( Member (State Code) r
    , Member (State Env) r
    , Member (State ValStack) r
    , Member (State CallStack) r
    ) => Eff r e


evalE :: ASMEval r Value
evalE = popM code >>= \case
    Nothing -> pop valstack
    -- Nothing -> do
    --     CallStack cs <- get callstack
    --     -- Catch missing returns
    --     if null cs
    --         then pop valstack
    --         else push code Return >> evalE
    Just c -> do
        get code >>= T.traceShowM
        get env >>= T.traceShowM
        get callstack >>= T.traceShowM
        get valstack >>= T.traceShowM
        T.traceM ("> " ++ show c ++ "\n")
        case c of
            Access i -> do
                Env e <- get env
                push valstack (e !! i)

            Apply -> do
                e <- get env
                cde <- get code
                push callstack (cde, e)

                Closure (cde', e') <- pop valstack
                put code cde'
                put env e'

                v <- pop valstack
                push env v

            Cur c' -> do
                e <- get env
                push valstack (Closure (Code c', e))

            Return -> do
                (c', e') <- pop callstack
                put code c'
                put env e'

            Let -> pop valstack >>= push env

            Endlet -> void $ pop env

            Branchneg i -> do
                Value (Expr.B v) <- pop valstack
                unless v $ replicateM_ i (pop code)

            Branch i -> replicateM_ i (pop code)

            Op op -> do
                Value v1 <- pop valstack
                Value v2 <- pop valstack
                push valstack (Value $ evalOp op v1 v2)

            Push v -> push valstack (Value v)
            >> evalE


eval :: Code -> Value
eval c = run $
    evalState c $
    evalState (Env []) $
    evalState (ValStack []) $
    evalState (CallStack [])
    evalE


example = Code [
        Push (Expr.I 5),
        Push (Expr.I 1),
        Let,
        Cur [
            Access 0,
            Access 1,
            Branch 1,
            Push (Expr.I $ -50),
            Push (Expr.B False),
            Branchneg 1,
            Push (Expr.I $ -1000),
            Op Plus,
            Return
        ],
        Apply,
        Push (Expr.I 7),
        Op Mult
    ]
