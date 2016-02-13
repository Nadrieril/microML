{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
-- DBT = De Bruijn Tree
module DBT.Expr
    ( Expr
    , AbstractExpr(..)
    , Id
    , LabelledExp
    , TypedExpr
    , deBruijn
    , calcVarName
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush)
import Common.Expr
import Common.Type
import qualified AFT.Expr as AFT

data AbstractExpr v a =
      Var v
    | Global Name
    | Const Value
    | If a a a
    | Fun Name a
    | Fix Name a
    | Let Name a a
    | Ap a a

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
                Ap f x -> printf "(%s %s)" (show f) (show x)
                Let x v e -> printf "let %s = %s in\n%s" (show x) (show v) (show e)
                If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
                Fun x e -> printf "(\\%s -> %s)" (show x) (show e)
                Fix x e -> printf "fix(\\%s -> %s)" (show x) (show e)


instance Show TypedExpr where
    show e@(LFixP t _) = printf "%s\n:: %s" (show $ calcVarName e) (show t)


instance Show (LabelledExp MonoType Name) where
    show (LFixP _ e) = case e of
        Var i -> show i
        Global x -> show x
        Const c -> show c
        Fun n e -> printf "(\\%s -> %s)" (show n) (show e)
        Fix n e -> printf "fix(\\%s -> %s)" (show n) (show e)
        Let n v e -> printf "let %s :: %s = %s in\n%s" (show n) (show $ label v) (show v) (show e)
        Ap f x -> printf "(%s %s)" (show f) (show x)
        If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)



calcVarName :: LabelledExp l Id -> LabelledExp l Name
calcVarName e = evalState (calcVarName'' e) []

calcVarName'' :: LabelledExp l Id -> State (Stack Name) (LabelledExp l Name)
calcVarName'' (LFixP l e) = LFixP l <$>
    case e of
        Var i -> do
            stk <- get
            return $ Var $ if i < length stk
                then stk !! i
                else Name $ printf "#%d" i
        Global x -> return $ Global x
        Const c -> return $ Const c
        Fun n e -> withPush n (Fun n <$> calcVarName'' e)
        Fix n e -> withPush n (Fix n <$> calcVarName'' e)
        Let n v e -> do
            vv <- calcVarName'' v
            withPush n (Let n <$> pure vv <*> calcVarName'' e)
        Ap f x -> Ap <$> calcVarName'' f <*> calcVarName'' x
        If b e1 e2 -> If <$> calcVarName'' b <*> calcVarName'' e1 <*> calcVarName'' e2


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
    AFT.Fun x e -> withPush x (Fun x <$> deBruijnE e)
    AFT.Fix f e -> withPush f (Fix f <$> deBruijnE e)
    AFT.Let x v e -> Let x <$> deBruijnE v <*> withPush x (deBruijnE e)

deBruijn :: AFT.Expr -> Expr
deBruijn e = evalState (deBruijnE e) []
