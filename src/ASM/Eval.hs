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
import Common.Expr (Id, Name(..))
import qualified Common.Expr as Expr (Value(..))
import qualified Common.StdLib as Std
import qualified Common.ADT as ADT
import ASM.Instr hiding (Env)
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
    | Constructor (ADT.ADT Id) Int Int [Value]
    | Deconstructor (ADT.ADT Id) Int [Value]
    | PartialSysCall (Expr.Value -> Std.StdLibValue)

instance Show Value where
    show (Value x) = printf "Value %s" (show x)
    -- show (Closure x) = printf "Closure %s" (show x)
    -- show (RecClosure x) = printf "RecClosure %s" (show x)
    show (Closure _) = printf "Closure"
    show (RecClosure _) = printf "RecClosure"
    show (Constructor adt n _ l) = let name = ADT.constructorName (ADT.adtConstructors adt !! n) in
        case l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (show x) (show name) (show y)
            l -> printf "%s%s" (show name) (show l)
    show (Deconstructor adt _ _) = printf "%s.." (show $ ADT.deconstructorName adt)
    show (PartialSysCall _) = printf "PartialSysCall"


getSysCall :: C.Context -> Name -> Value
getSysCall ctx x | Just (cv, _) <- x `M.lookup` ctx =
    case cv of
        C.Value v -> Value v
        C.Constructor adt n i -> Constructor adt n i []
        C.Deconstructor adt i -> Deconstructor adt i []
        C.SysCall sc -> PartialSysCall sc
getSysCall _ x = error $ printf "Unknown syscall: %s" (show x)



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

    Constructor adt n i l | i /= 0 -> do
        x <- pop valstack
        push valstack $ Constructor adt n (i-1) (x:l)

    Deconstructor adt 0 p -> do
        let n = ADT.adtName adt
        pop valstack >>= \case
            Constructor (ADT.adtName -> n') _ 0 _ | n /= n' -> error $ printf "Attempting to deconstruct %s value as a %s" (show n) (show n')
            Constructor _ n 0 l -> do
                let f = p !! (length p - 1 - n)
                forM_ l (push valstack)
                push valstack f
                forM_ l (\_ -> push code Apply)
            v -> error $ printf "Attempting to deconstruct non-product value %s" (show v)
    Deconstructor adt i p -> do
        x <- pop valstack
        push valstack $ Deconstructor adt (i-1) (x:p)

    PartialSysCall f -> do
            Value v <- pop valstack
            push valstack $ case f v of
                Std.Val v' -> Value v'
                Std.Fun f' -> PartialSysCall f'

    v -> error $ printf "Error: attempting to evaluate %s as a function" (show v)


evalInstr :: Instr -> ASMEval r ()
evalInstr c = case c of
    Access i -> do
        Env e <- get env
        push valstack (e !! i)

    Apply -> pop valstack >>= evalAp

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

    SysCall sc -> push valstack (getSysCall C.globalContext sc)

    Push v -> push valstack (Value v)


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
                else push code Return >> evalE

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
