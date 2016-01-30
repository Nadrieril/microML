{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
module Typed.Expr
    ( VId
    , Name(..)
    , Value(..)
    , AExpr(..)
    , TExpr(..)
    , Expr
    ) where

import Text.Printf (printf)
import Control.Monad.State.Strict (State, gets, evalState)

import Utils (Stack, local, push)
import DeBruijn.Expr (Name(..), Value(..), AExpr(..))
import Typed.Type (Type)

type VId = Int

data TExpr f = TExpr { typ :: Type, unTExpr :: f (TExpr f) }

type Expr = TExpr AExpr


instance Show Expr where
  show e@(TExpr t _) = printf "%s\n:: %s" (evalState (showE e) []) (show t)

showE :: Expr -> State (Stack Name) String
showE (unTExpr -> Var i) = show <$> gets (!! i)
showE (unTExpr -> Global x) = return $ show x
showE (unTExpr -> Const c) = return $ show c
showE (unTExpr -> Fun n e) = local $ do
        push n
        printf "(\\%s -> %s)" (show n) <$> showE e
showE (unTExpr -> Fix n e) = local $ do
        push n
        printf "fix(\\%s -> %s)" (show n) <$> showE e
showE (unTExpr -> Let n v e) = local $ do
        let TExpr t _ = v
        push n
        printf "let %s :: %s = %s in\n%s" (show n) (show t) <$> showE v <*> showE e
showE (unTExpr -> Ap f x) = printf "(%s %s)" <$> showE f <*> showE x
showE (unTExpr -> If b e1 e2) = printf "if %s then %s else %s" <$> showE b <*> showE e1 <*> showE e2
showE _ = error "impossible"
