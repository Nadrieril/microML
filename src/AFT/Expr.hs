{-# LANGUAGE DeriveFunctor #-}
module AFT.Expr
    ( AST.Name(..)
    , AST.Value(..)
    , Expr(..)
    , fromAST
    ) where

import Text.Printf (printf)

import AST.Expr (Value)
import qualified AST.Expr as AST

data Expr a =
      Var a
    | Const Value
    | Fun a (Expr a)
    | Fix a (Expr a)
    | Ap (Expr a) (Expr a)
    | Let a (Expr a) (Expr a)
    | If (Expr a) (Expr a) (Expr a)
    deriving (Functor)

instance Show a => Show (Expr a) where
  show (Var x) = show x
  show (Const c) = show c
  show (Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
  show (Fix f e) = printf "fix(\\%s -> %s)" (show f) (show e)
  show (Ap f x) = printf "(%s %s)" (show f) (show x)
  show (Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
  show (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)

fromAST :: AST.Expr a -> Expr a
fromAST (AST.Var v) = Var v
fromAST (AST.Const c) = Const c
fromAST (AST.Fun n e) = Fun n (fromAST e)
fromAST (AST.Ap f x) = Ap (fromAST f) (fromAST x)
fromAST (AST.If b e1 e2) = If (fromAST b) (fromAST e1) (fromAST e2)
fromAST (AST.Infix o e1 e2) = Ap (Ap (Var o) (fromAST e1)) (fromAST e2)
fromAST (AST.Let x v e) = Let x (fromAST v) (fromAST e)
fromAST (AST.LetR f v e) = Let f (Fix f (fromAST v)) (fromAST e)
