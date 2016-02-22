{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, PatternSynonyms #-}
-- DBT = De Bruijn Tree
module DBT.Expr
    ( Expr
    , AbstractExpr(..)
    , Scope(..)
    , Id
    , LabelledExp
    , TypedExpr
    , deBruijn
    , calcVarName
    , pattern SFun , pattern SFix , pattern SLet
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, evalState)

import Utils (Stack, withPush)
import AST.Parse (isOperator)
import Common.Expr
import Common.Type
import qualified AFT.Expr as AFT

data AbstractExpr v a =
      Var v
    | Global Name
    | Const Value
    | If a a a
    | Fun (Scope a)
    | Fix (Scope a)
    | Let a (Scope a)
    | Ap a a

data Scope e = Scope Name e

pattern SFun e <- Fun (Scope _ e)
pattern SFix e <- Fix (Scope _ e)
pattern SLet v e <- Let v (Scope _ e)

type LabelledExp l v = LFixP l (AbstractExpr v)

type Id = Int

type Expr = LabelledExp (Maybe (Mono Name)) Id
type TypedExpr = LabelledExp MonoType Id


instance Show Expr where
  show =  show . calcVarName

instance Show (LabelledExp (Maybe (Mono Name)) Name) where
    show (LFixP t e) = case t of
            Nothing -> s
            Just t -> printf "%s :: %s" s (show t)
        where s = case e of
                Var x -> show x
                Global x -> show x
                Const c -> show c
                Ap (expr -> Ap (expr -> Var o) x) y | isOperator o -> printf "(%s %s %s)" (show x) (show o) (show y)
                Ap f x -> printf "(%s %s)" (show f) (show x)
                Let v (Scope x e) -> printf "let %s = %s in\n%s" (show x) (show v) (show e)
                If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
                Fun (Scope x e) -> printf "(\\%s -> %s)" (show x) (show e)
                Fix (Scope x e) -> printf "fix(\\%s -> %s)" (show x) (show e)


instance Show TypedExpr where
    show e@(LFixP t _) = printf "%s\n:: %s" (show $ calcVarName e) (show t)


instance Show (LabelledExp MonoType Name) where
    show (LFixP _ e) = case e of
        Var i -> showIdent i
        Global x -> showIdent x
        Const c -> show c
        Fun (Scope n e) -> printf "(\\%s -> %s)" (show n) (show e)
        Fix (Scope n e) -> printf "fix(\\%s -> %s)" (show n) (show e)
        Let v (Scope n e) -> let (params, v') = unfoldFun v in
            let paramStr = concatMap ((:) ' ' . show) params in
            printf "let %s%s = %s :: %s in\n%s" (showIdent n) paramStr (show v') (show $ label v) (show e)
        Ap (expr -> Ap (expr -> Var o) x) y | isOperator o -> printf "(%s %s %s)" (show x) (show o) (show y)
        Ap f x@(expr -> Ap _ _) -> printf "%s (%s)" (show f) (show x)
        Ap f x -> printf "%s %s" (show f) (show x)
        If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
        where
            showIdent n = (if isOperator n then printf "(%s)" else id) (show n)
            unfoldFun (expr -> Fun (Scope n e)) =
                let (l, e') = unfoldFun e in
                (n:l, e')
            unfoldFun x = ([], x)



calcVarName :: LabelledExp l Id -> LabelledExp l Name
calcVarName e = evalState (calcVarName' e) []

calcVarName' :: LabelledExp l Id -> State (Stack Name) (LabelledExp l Name)
calcVarName' (LFixP l e) = LFixP l <$>
    case e of
        Var i -> do
            stk <- get
            return $ Var $ if i < length stk
                then stk !! i
                else Name $ printf "#%d" i
        Global x -> return $ Global x
        Const c -> return $ Const c
        Fun s -> Fun <$> auxScope s
        Fix s -> Fix <$> auxScope s
        Let v s -> Let <$> calcVarName' v <*> auxScope s
        Ap f x -> Ap <$> calcVarName' f <*> calcVarName' x
        If b e1 e2 -> If <$> calcVarName' b <*> calcVarName' e1 <*> calcVarName' e2
        where auxScope (Scope n e) = Scope n <$> withPush n (calcVarName' e)


deBruijnE :: AFT.Expr -> State (Stack Name) Expr
deBruijnE (LFixP t e) = LFixP t <$> case e of
    AFT.Var x -> do
        s <- get
        return $ case elemIndex x s of
            Just i -> Var i
            Nothing -> Global x
    AFT.Const c -> return $ Const c
    AFT.If b e1 e2 -> If <$> deBruijnE b <*> deBruijnE e1 <*> deBruijnE e2
    AFT.Ap f x -> Ap <$> deBruijnE f <*> deBruijnE x
    AFT.Fun s -> Fun <$> auxScope s
    AFT.Fix s -> Fix <$> auxScope s
    AFT.Let v s -> Let <$> deBruijnE v <*> auxScope s
    where auxScope (AFT.Scope n e) = Scope n <$> withPush n (deBruijnE e)

deBruijn :: AFT.Expr -> Expr
deBruijn e = evalState (deBruijnE e) []
