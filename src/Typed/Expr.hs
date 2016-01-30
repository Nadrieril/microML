{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Typed.Expr
    ( VId
    , Name(..)
    , Value(..)
    , TExpr(..)
    , AbsExpr(..)
    , Expr
    ) where

import Text.Printf (printf)
import Control.Monad.State.Strict (State, gets, evalState)

import Utils (Stack, local, push)
import DeBruijn.Expr (Name(..), Value(..), AbsExpr(..), AExpr)

type VId = Int

data TExpr t e = TExpr t e

type Expr a = AExpr (TExpr a)


instance Show a => Show (Expr a) where
  show e@(TExpr t _) = printf "%s\n:: %s" (evalState (showTE e) []) (show t)

showTE :: Show a => TExpr a (AbsExpr (TExpr a)) -> State (Stack Name) String
showTE (TExpr _ e) = showE e

showE :: Show a => AbsExpr (TExpr a) -> State (Stack Name) String
showE (Var i) = show <$> gets (!! i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun n e) = local $ do
        push n
        printf "(\\%s -> %s)" (show n) <$> showTE e
showE (Fix n e) = local $ do
        push n
        printf "fix(\\%s -> %s)" (show n) <$> showTE e
showE (Let n v e) = local $ do
        let TExpr t _ = v
        push n
        printf "let %s :: %s = %s in\n%s" (show n) (show t) <$> showTE v <*> showTE e
showE (Ap f x) = printf "(%s %s)" <$> showTE f <*> showTE x
showE (If b e1 e2) = printf "if %s then %s else %s" <$> showTE b <*> showTE e1 <*> showTE e2
