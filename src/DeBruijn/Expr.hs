{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances, PatternSynonyms, ViewPatterns #-}
module DeBruijn.Expr
    ( FixP(..)
    , Name(..)
    , Value(..)
    , Expr
    , AExpr(..)
    , Id
    , deBruijn
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush, local, push)
import AFT.Expr (Name, Value)
import qualified AFT.Expr as AFT

data AExpr a =
      Var Id
    | Global Name
    | Const Value
    | If a a a
    | Fun Name a
    | Fix Name a
    | Let Name a a
    | Ap a a

newtype FixP f = FixP {unFixP :: f (FixP f)}

type Id = Int

type Expr = FixP AExpr









instance Show Expr where
  show e =  evalState (showE e) []

showE :: Expr -> State (Stack Name) String
showE (unFixP -> Var i) = show <$> gets (!! i)
showE (unFixP -> Global x) = return $ show x
showE (unFixP -> Const c) = return $ show c
showE (unFixP -> Fun n e) = local $ do
        push n
        printf "(\\%s -> %s)" (show n) <$> showE e
showE (unFixP -> Fix n e) = local $ do
        push n
        printf "fix(\\%s -> %s)" (show n) <$> showE e
showE (unFixP -> Let n v e) = local $ do
        push n
        printf "let %s = %s in\n%s" (show n) <$> showE v <*> showE e
showE (unFixP -> Ap f x) = printf "(%s %s)" <$> showE f <*> showE x
showE (unFixP -> If b e1 e2) = printf "if %s then %s else %s" <$> showE b <*> showE e1 <*> showE e2
showE _ = error "impossible"


deBruijnAE :: AFT.Expr Name -> State (Stack Name) Expr
deBruijnAE = fmap FixP . deBruijnE

deBruijnE :: AFT.Expr Name -> State (Stack Name) (AExpr Expr)
deBruijnE (AFT.Var x) = do
    s <- get
    return $ case elemIndex x s of
        Just i -> Var i
        Nothing -> Global x
deBruijnE (AFT.Const c) = return $ Const c
deBruijnE (AFT.If b e1 e2) = If <$> deBruijnAE b <*> deBruijnAE e1 <*> deBruijnAE e2
deBruijnE (AFT.Ap f x) = Ap <$> deBruijnAE f <*> deBruijnAE x
deBruijnE (AFT.Fun x e) = withPush x (Fun x <$> deBruijnAE e)
deBruijnE (AFT.Fix f e) = withPush f (Fix f <$> deBruijnAE e)
deBruijnE (AFT.Let x v e) = Let x <$> deBruijnAE v <*> withPush x (deBruijnAE e)

deBruijn :: AFT.Expr Name -> Expr
deBruijn e = evalState (deBruijnAE e) []
