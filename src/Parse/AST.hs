-- AST = Abstract Syntax Tree
module Parse.AST
    ( AbstractExpr(..)
    , LabelledExp
    , TypedExpr
    , UntypedExpr
    , Expr
    , Program
    ) where

import Common.Expr
import Common.ADT
import Common.Pattern
import Common.Type


data AbstractExpr a =
      Var Name
    | Const Value
    | Infix Name a a
    | Negate a
    | Ap a a
    | Let Name [Name] a a
    | LetR Name [Name] a a
    | Match a [(Pattern Name, a)]
    | If a a a
    | Fun Name a
    | Wrap a

type LabelledExp l = LFixP AbstractExpr l

type TypedExpr = LabelledExp (Maybe (Mono Name))
type UntypedExpr = AbstractExpr TypedExpr
type Expr = TypedExpr

type Program = ([ADT Name], Expr)
