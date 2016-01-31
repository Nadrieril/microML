{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AST.Expr
    ( Name(..)
    , Value(..)
    , AExpr(..)
    , LFixP(..)
    , LExp
    , Expr
    , TExpr
    ) where

import Text.Printf (printf)

import Typed.Type


newtype Name = Name String
    deriving (Eq, Ord)
instance Show Name where
  show (Name o) = o

data Value = B Bool | I Integer
instance Show Value where
  show (B x) = show x
  show (I x) = show x


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }

data AExpr v a =
      Var v
    | Const Value
    | Infix v a a
    | Ap a a
    | Let v a a
    | LetR v a a
    | If a a a
    | Fun v a
    | Wrap a

type LExp l v = LFixP l (AExpr v)

type TExpr v = LExp (Maybe (Mono Name)) v
type Expr v = AExpr v (TExpr v)


instance Show v => Show (TExpr v) where
    show (LFixP Nothing e) = showE e
    show (LFixP (Just t) e) = printf "%s :: %s" (showE e) (show t)

showE :: Show v => AExpr v (TExpr v) -> String
showE (Var x) = show x
showE (Const c) = show c
showE (Infix o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
showE (Ap f x) = printf "(%s %s)" (show f) (show x)
showE (Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
showE (LetR x v e) = printf "let rec %s = %s in\n%s" (show x) (show v) (show e)
showE (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
showE (Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
showE (Wrap e) = printf "(%s)" (show e)
