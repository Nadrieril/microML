{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module ASM.Eval where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import qualified Debug.Trace as T
import Control.Monad (replicateM_, void, unless)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, evalState)
import Utils.ProxyStateEff (get, put, modify)

-- import qualified Utils (trace)
import Common.Expr (SysCall(..))
import qualified Common.Expr as Expr (Value(..))
import qualified Common.StdLib as Std
import ASM.Instr hiding (Env)

-- trace :: Show a => a -> b -> b
-- trace = Utils.trace True

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


data Value = Value Expr.Value | Closure (Code, Env) | RecClosure (Code, Env)
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


evalStd :: Std.StdLibValue -> ASMEval r Value
evalStd (Std.Val v) = return (Value v)
evalStd (Std.Fun f) = do
    Value v <- pop valstack
    evalStd (f v)

evalSysCall :: SysCall -> ASMEval r Value
evalSysCall sc = evalStd $ fst $ Std.sysCallToValue sc


type ASMEval r e =
    ( Member (State Code) r
    , Member (State Env) r
    , Member (State ValStack) r
    , Member (State CallStack) r
    ) => Eff r e


evalE :: ASMEval r Value
evalE = do
    get code >>= T.traceShowM
    get env >>= T.traceShowM
    get callstack >>= T.traceShowM
    get valstack >>= T.traceShowM

    popM code >>= \case
        Nothing -> do
            CallStack cs <- get callstack
            -- Catch missing returns
            if null cs
                then pop valstack
                else push code Return >> evalE

        Just c -> do
            T.traceM ("> " ++ show c ++ "\n")
            case c of
                Access i -> do
                    Env e <- get env
                    push valstack (e !! i)

                Apply -> do
                    e <- get env
                    cde <- get code
                    push callstack (cde, e)

                    pop valstack >>= \case
                        Closure (cde', e') -> do
                            put code cde'
                            put env e'
                        RecClosure (cde', e') -> do
                            put code cde'
                            put env e'
                            push env (RecClosure (cde', e'))
                        _ -> return ()

                    v <- pop valstack
                    push env v

                Cur c' -> do
                    e <- get env
                    push valstack (Closure (Code c', e))

                Rec c' -> do
                    e <- get env
                    push valstack (RecClosure (Code c', e))

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

                SysCall sc -> evalSysCall sc >>= push valstack

                Push v -> push valstack (Value v)

            evalE


eval :: Code -> Value
eval c = run $
    evalState c $
    evalState (Env []) $
    evalState (ValStack []) $
    evalState (CallStack [])
    evalE
