{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module DeBruijn.Expr
    ( Expr
    , AbstractExpr(..)
    , Id
    , LabelledExp
    , deBruijn
    , calcVarName
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush, local, push)
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

type TypedExpr v = LabelledExp (Maybe (Mono Name)) v
type UntypedExpr v = AbstractExpr v (TypedExpr v)
type Expr = TypedExpr Id


instance Show Expr where
  show =  show . calcVarName


instance Show (TypedExpr Name) where
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


calcVarName :: LabelledExp l Id -> LabelledExp l Name
calcVarName e = evalState (calcVarName'' e) []

calcVarName'' :: LabelledExp l Id -> State (Stack Name) (LabelledExp l Name)
calcVarName'' (LFixP l e) = LFixP l <$> calcVarName' e

calcVarName' :: AbstractExpr Id (LabelledExp l Id) -> State (Stack Name) (AbstractExpr Name (LabelledExp l Name))
calcVarName' (Var i) = Var <$> gets (!! i)
-- calcVarName' (Var i) = do
--     AFT.Name n <- gets (!! i)
--     return $ Var $ AFT.Name $ printf "#%d,%s" i n
calcVarName' (Global x) = return $ Global x
calcVarName' (Const c) = return $ Const c
calcVarName' (Fun n e) = local $ do
        push n
        Fun n <$> calcVarName'' e
calcVarName' (Fix n e) = local $ do
        push n
        Fix n <$> calcVarName'' e
calcVarName' (Let n v e) = do
    vv <- calcVarName'' v
    local $ do
        push n
        Let n <$> pure vv <*> calcVarName'' e
calcVarName' (Ap f x) = Ap <$> calcVarName'' f <*> calcVarName'' x
calcVarName' (If b e1 e2) = If <$> calcVarName'' b <*> calcVarName'' e1 <*> calcVarName'' e2



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
