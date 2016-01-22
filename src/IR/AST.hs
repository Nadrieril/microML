{-# LANGUAGE DeriveFunctor #-}
module IR.AST
    ( Name(..)
    , Value(..)
    , Expr(..)
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

data Expr a =
      Var a
    | Const Value
    | Infix a (Expr a) (Expr a)
    | Ap (Expr a) (Expr a)
    | Let a (Expr a) (Expr a)
    | LetR a (Expr a) (Expr a)
    | If (Expr a) (Expr a) (Expr a)
    | Fun a (Expr a)
    deriving (Functor)

instance Show a => Show (Expr a) where
  show (Var x) = show x
  show (Const c) = show c
  show (Infix o x y) = printf "(%s %s %s)" (show x) (show o) (show y)
  show (Ap f x) = printf "(%s %s)" (show f) (show x)
  show (Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
  show (LetR x v e) = printf "let rec %s = %s in\n%s" (show x) (show v) (show e)
  show (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
  show (Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
