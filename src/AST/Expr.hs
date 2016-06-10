{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- ASM = Abstract Syntax Tree
module AST.Expr
    ( AbstractExpr(..)
    , LabelledExp
    , TypedExpr
    , UntypedExpr
    , Expr
    , Program
    ) where

import Common.Expr
import Common.ADT
import Common.Type


data AbstractExpr v a =
      Var v
    | Const Value
    | Infix v a a
    | Negate a
    | Ap a a
    | Let v a a
    | LetR v a a
    | If a a a
    | Fun v a
    | Wrap a

type LabelledExp l v = LFixP (AbstractExpr v) l

type TypedExpr v = LabelledExp (Maybe (Mono Name)) v
type UntypedExpr v = AbstractExpr v (TypedExpr v)
type Expr = TypedExpr Name

type Program = ([ADT Name], Expr)
