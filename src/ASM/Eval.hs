{-# LANGUAGE RankNTypes, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module ASM.Eval where

import Data.Proxy (Proxy(..))
import qualified Data.Map as M
import qualified Debug.Trace as T
import Data.List (intercalate)
import Control.Monad (void, unless, forM_, forM, when)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Text.Printf (printf)

-- import qualified Utils (trace)
import Parse.Token (isOperator)
import Common.Expr (Name)
import qualified Common.Expr as Expr (Value(..))
import qualified Common.StdLib as Std
import qualified Common.ADT as ADT
import ASM.Instr (Instr)
import qualified ASM.Instr as I
import qualified Common.Context as C
import Utils.PrettyPrint
import Utils.ProxyStateEff (get, put)
import Utils.Stackable


data Value =
      Value Expr.Value
    | Closure (Code, Env)
    | RecClosure (Code, Env)
    | Constructor Name Int Int [Value]
    | Deconstructor Name Int [Value]
    | PartialSysCall (Expr.Value -> C.ContextValue)

instance PrettyPrint Value where
    pprint (Value x) = printf "Value %s" (pprint x)
    pprint (Closure _) = printf "Closure"
    pprint (RecClosure _) = printf "RecClosure"
    pprint (Constructor name _ _ l) = case reverse l of
            [x, y] | isOperator name -> printf "(%s %s %s)" (pprint x) (pprint name) (pprint y)
            l -> printf "%s[%s]" (pprint name) (intercalate ", " (map pprint l))
    pprint (Deconstructor name _ _) = printf "un%s[..]" (pprint name)
    pprint (PartialSysCall _) = printf "PartialSysCall"

instance Show Value where
    show = let ?toplevel = False in pprint

fromContext :: C.ContextValue -> Value
fromContext = \case
    C.Value v -> Value v
    C.Constructor adt n i -> let name = ADT.constructorName (ADT.adtConstructors adt !! n) in
            Constructor name n i []
    C.Deconstructor adt i -> Deconstructor (ADT.adtName adt) i []
    C.SysCall sc -> PartialSysCall sc

getSysCall :: Name -> Value
getSysCall x
    | Just (cv, _) <- x `M.lookup` Std.globalContext = fromContext cv
    | otherwise = error $ printf "Unknown syscall: %s" (show x)



newtype Code = Code (Stack Instr)
    deriving (Show, Stackable Instr)
newtype Env = Env (Stack Value)
    deriving (Show, Stackable Value)
newtype ValStack = ValStack (Stack Value)
    deriving (Show, Stackable Value)
newtype CallStack = CallStack (Stack (Code, Env))
    deriving (Show, Stackable (Code, Env))

code = Proxy :: Proxy Code
env = Proxy :: Proxy Env
valstack = Proxy :: Proxy ValStack
callstack = Proxy :: Proxy CallStack


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
        push valstack $ fromContext $ f v

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

    I.Branchmatch p i -> do
        let matchVal :: Value -> I.Pattern -> Maybe [Value]
            matchVal (Constructor n _ _ vals) (I.Pattern n' pats)
                | n == n' = do
                    matches <- forM (zip (reverse vals) pats) $ uncurry matchVal
                    return $ concat matches
            matchVal x I.PVar = Just [x]
            matchVal _ _ = Nothing

        v <- pop valstack
        case matchVal v p of
            Just vals -> do
                pushAll env vals
                dropS code i
            Nothing -> push valstack v

    I.Branchneg i -> do
        Value (Expr.B v) <- pop valstack
        unless v $ dropS code i

    I.Branch i -> dropS code i

    I.SysCall sc -> push valstack $ getSysCall sc

    I.Constructor name n i -> push valstack $ Constructor name n i []

    I.Deconstructor name i -> push valstack $ Deconstructor name i []

    I.Push v -> push valstack (Value v)

    I.Panic s -> error s


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
            when debug $ T.traceM ("\n> " ++ show c ++ "\n")
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
