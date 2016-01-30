{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module DeBruijn.Expr
    ( Identity(..)
    , Name(..)
    , Value(..)
    , Expr
    , AbsExpr(..)
    , AExpr
    , Id
    , deBruijn
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush, local, push)
import AFT.Expr (Name, Value)
import qualified AFT.Expr as AFT

newtype Identity a = Identity { runIdentity :: a}
    deriving (Functor)

data AbsExpr f =
      Var Id
    | Global Name
    | Const Value
    | If (AExpr f) (AExpr f) (AExpr f)
    | Fun Name (AExpr f)
    | Fix Name (AExpr f)
    | Let Name (AExpr f) (AExpr f)
    | Ap (AExpr f) (AExpr f)

type AExpr f = f (AbsExpr f)


type Id = Int

type Expr = AExpr Identity

instance Show Expr where
  show e =  evalState (showAE e) []

showAE :: Expr -> State (Stack Name) String
showAE = showE . runIdentity

showE :: AbsExpr Identity -> State (Stack Name) String
showE (Var i) = show <$> gets (!! i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun n e) = local $ do
        push n
        printf "(\\%s -> %s)" (show n) <$> showAE e
showE (Fix n e) = local $ do
        push n
        printf "fix(\\%s -> %s)" (show n) <$> showAE e
showE (Let n v e) = local $ do
        push n
        printf "let %s = %s in\n%s" (show n) <$> showAE v <*> showAE e
showE (Ap f x) = printf "(%s %s)" <$> showAE f <*> showAE x
showE (If b e1 e2) = printf "if %s then %s else %s" <$> showAE b <*> showAE e1 <*> showAE e2


deBruijnAE :: AFT.Expr Name -> State (Stack Name) Expr
deBruijnAE = fmap Identity . deBruijnE

deBruijnE :: AFT.Expr Name -> State (Stack Name) (AbsExpr Identity)
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
