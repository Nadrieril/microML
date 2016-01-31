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

type TExpr v = LExp (Maybe ()) v
type Expr v = AExpr v (TExpr v)


instance Show v => Show (TExpr v) where
  show (expr -> Var x) = show x
  show (expr -> Const c) = show c
  show (expr -> Infix o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
  show (expr -> Ap f x) = printf "(%s %s)" (show f) (show x)
  show (expr -> Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
  show (expr -> LetR x v e) = printf "let rec %s = %s in\n%s" (show x) (show v) (show e)
  show (expr -> If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
  show (expr -> Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
  show (expr -> Wrap e) = printf "(%s)" (show e)
  show _ = error "impossible"
