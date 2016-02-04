{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AST.Expr
    ( AbstractExpr(..)
    , LabelledExp
    , TypedExpr
    , UntypedExpr
    , Expr
    ) where

import Text.Printf (printf)

import Common.Expr
import Common.Type


data AbstractExpr v a =
      Var v
    | Const Value
    | Infix v a a
    | Ap a a
    | Let v a a
    | LetR v a a
    | If a a a
    | Fun v a
    | Wrap a

type LabelledExp l v = LFixP l (AbstractExpr v)

type TypedExpr v = LabelledExp (Maybe (Mono Name)) v
type UntypedExpr v = AbstractExpr v (TypedExpr v)
type Expr = TypedExpr Name

instance Show v => Show (TypedExpr v) where
    show (LFixP Nothing e) = showE e
    show (LFixP (Just t) e) = printf "%s :: %s" (showE e) (show t)

showE :: Show v => AbstractExpr v (TypedExpr v) -> String
showE (Var x) = show x
showE (Const c) = show c
showE (Infix o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
showE (Ap f x) = printf "(%s %s)" (show f) (show x)
showE (Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
showE (LetR x v e) = printf "let rec %s = %s in\n%s" (show x) (show v) (show e)
showE (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
showE (Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
showE (Wrap e) = printf "(%s)" (show e)
