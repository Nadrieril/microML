{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Typed.Expr
    ( VId
    , Name(..)
    , Value(..)
    , Expr(..)
    , SExpr(..)
    ) where

import Text.Printf (printf)
import Data.Char (chr, ord)
import Control.Monad.State.Strict (State, gets, modify, evalState)

import Utils (local)
import DeBruijn.Expr (Name(..), Value(..))

type VId = Int

data Expr a = TExpr a (SExpr a)
    deriving (Functor, Foldable, Traversable)

data SExpr a =
      Var VId
    | Global Name
    | Const Value
    | Fun (Expr a)
    | Fix (Expr a)
    | Ap (Expr a) (Expr a)
    | Let (Expr a) (Expr a)
    | If (Expr a) (Expr a) (Expr a)
    deriving (Functor, Foldable, Traversable)


instance Show a => Show (Expr a) where
  show e@(TExpr t _) = printf "%s\n:: %s" (evalState (showTE e) (-1)) (show t)

showVar :: Int -> String
showVar i = [chr (i + ord 'a')]

showTE :: Show a => Expr a -> State Int String
showTE (TExpr _ e) = showE e

showE :: Show a => SExpr a -> State Int String
showE (Var i) = showVar <$> gets (\j -> j-i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun e) = local $ do
        modify (+ 1)
        printf "(\\%s -> %s)" <$> gets showVar <*> showTE e
showE (Fix e) = local $ do
        modify (+ 1)
        printf "fix(\\%s -> %s)" <$> gets showVar <*> showTE e
showE (Let v e) = local $ do
        modify (+ 1)
        let TExpr t _ = v
        printf "let %s :: %s = %s in\n%s" <$> gets showVar <*> pure (show t) <*> showTE v <*> showTE e
showE (Ap f x) = printf "(%s %s)" <$> showTE f <*> showTE x
showE (If b e1 e2) = printf "if %s then %s else %s" <$> showTE b <*> showTE e1 <*> showTE e2
