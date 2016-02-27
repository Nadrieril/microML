{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module ASM.Eval where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Debug.Trace as T
import Control.Monad (replicateM_, void, unless, forM_, when)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Utils.ProxyStateEff (get, put, modify)
import Text.Printf (printf)

-- import qualified Utils (trace)
import AST.Parse (isOperator)
import Common.Expr (Name(..))
import qualified Common.Expr as Expr (Value(..))
import qualified Common.StdLib as Std
import qualified Common.ADT as ADT
import ASM.Instr (Instr)
import qualified ASM.Instr as I
import qualified Common.Context as C


type Stack a = [a]

class Stackable b a | a -> b where
    wrapStack :: Stack b -> a
    unwrapStack :: a -> Stack b

instance Stackable a (Stack a) where
    wrapStack = id
    unwrapStack = id

inStack :: Stackable b a => (Stack b -> Stack b) -> a -> a
inStack = (wrapStack .) . (. unwrapStack)

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


data Value =
      Value Expr.Value
    | Closure (Code, Env)
    | RecClosure (Code, Env)
    | Constructor Name Int Int [Value]
    | Deconstructor Name Int [Value]
    | PartialSysCall (Expr.Value -> Std.StdLibValue)

instance Show Value where
    show (Value x) = printf "Value %s" (show x)
    show (Closure _) = printf "Closure"
    show (RecClosure _) = printf "RecClosure"
    show (Constructor name _ _ l) = case l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (show x) (show name) (show y)
            l -> printf "%s%s" (show name) (show l)
    show (Deconstructor name _ _) = printf "un%s[..]" (show name)
    show (PartialSysCall _) = printf "PartialSysCall"


getSysCall :: Name -> Value
getSysCall x
    | Just (cv, _) <- x `M.lookup` C.globalContext  =
        case cv of
            C.Value v -> Value v
            C.Constructor adt n i -> let name = ADT.constructorName (ADT.adtConstructors adt !! n) in
                    Constructor name n i []
            C.Deconstructor adt i -> Deconstructor (ADT.adtName adt) i []
            C.SysCall sc -> PartialSysCall sc
    | otherwise = error $ printf "Unknown syscall: %s" (show x)



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


type ASMEval r e =
    ( Member (Reader Bool) r
    , Member (State Code) r
    , Member (State Env) r
    , Member (State ValStack) r
    , Member (State CallStack) r
    ) => Eff r e


evalAp :: Value -> ASMEval r ()
evalAp = \case
    Closure (cde, e) -> do
        (,) <$> get code <*> get env >>= push callstack
        put code cde
        put env e
        pop valstack >>= push env

    RecClosure (cde, e) -> do
        let e' = inStack (RecClosure (cde, e) :) e
        evalAp $ Closure (cde, e')

    Constructor name n i l | i /= 0 -> do
        x <- pop valstack
        push valstack $ Constructor name n (i-1) (x:l)

    Deconstructor _ 0 p ->
        pop valstack >>= \case
            Constructor _ n 0 l -> do
                let f = p !! (length p - 1 - n)
                forM_ l (push valstack)
                push valstack f
                forM_ l (\_ -> push code I.Apply)
            v -> error $ printf "Attempting to deconstruct non-product value %s" (show v)
    Deconstructor name i p -> do
        x <- pop valstack
        push valstack $ Deconstructor name (i-1) (x:p)

    PartialSysCall f -> do
            Value v <- pop valstack
            push valstack $ case f v of
                Std.Val v' -> Value v'
                Std.Fun f' -> PartialSysCall f'

    v -> error $ printf "Error: attempting to evaluate %s as a function" (show v)


evalInstr :: Instr -> ASMEval r ()
evalInstr c = case c of
    I.Access i -> do
        Env e <- get env
        push valstack (e !! i)

    I.Apply -> pop valstack >>= evalAp

    I.Cur c' -> do
        e <- get env
        push valstack (Closure (Code c', e))

    I.Rec c' -> do
        e <- get env
        push valstack (RecClosure (Code c', e))

    I.Return -> do
        (c', e') <- pop callstack
        put code c'
        put env e'

    I.Let -> pop valstack >>= push env

    I.Endlet -> void $ pop env

    I.Branchneg i -> do
        Value (Expr.B v) <- pop valstack
        unless v $ replicateM_ i (pop code)

    I.Branch i -> replicateM_ i (pop code)

    I.SysCall sc -> push valstack $ getSysCall sc

    I.Constructor name n i -> push valstack $ Constructor name n i []

    I.Deconstructor name i -> push valstack $ Deconstructor name i []

    I.Push v -> push valstack (Value v)


evalE :: ASMEval r Value
evalE = do
    debug <- ask
    when debug $ do
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
                else push code I.Return >> evalE

        Just c -> do
            when debug $ T.traceM ("> " ++ show c ++ "\n")
            evalInstr c
            evalE


eval :: Bool -> Code -> Value
eval debug c = run $
    flip runReader debug $
    evalState c $
    evalState (Env []) $
    evalState (ValStack []) $
    evalState (CallStack [])
    evalE
