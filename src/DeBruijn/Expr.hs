module DeBruijn.Expr
    ( Name(..)
    , Value(..)
    , Expr(..)
    , Id
    , deBruijn
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush, local, push)
import AFT.Expr (Name, Value)
import qualified AFT.Expr as AFT


type Id = Int

data Expr =
      Var Id
    | Global Name
    | Const Value
    | If Expr Expr Expr
    | Fun Name Expr
    | Fix Name Expr
    | Let Name Expr Expr
    | Ap Expr Expr

instance Show Expr where
  show e =  evalState (showE e) []

showE :: Expr -> State (Stack Name) String
showE (Var i) = show <$> gets (!! i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun n e) = local $ do
        push n
        printf "(\\%s -> %s)" (show n) <$> showE e
showE (Fix n e) = local $ do
        push n
        printf "fix(\\%s -> %s)" (show n) <$> showE e
showE (Let n v e) = local $ do
        push n
        printf "let %s = %s in\n%s" (show n) <$> showE v <*> showE e
showE (Ap f x) = printf "(%s %s)" <$> showE f <*> showE x
showE (If b e1 e2) = printf "if %s then %s else %s" <$> showE b <*> showE e1 <*> showE e2



deBruijnE :: AFT.Expr Name -> State (Stack Name) Expr
deBruijnE (AFT.Var x) = do
    s <- get
    return $ case elemIndex x s of
        Just i -> Var i
        Nothing -> Global x
deBruijnE (AFT.Const c) = return $ Const c
deBruijnE (AFT.If b e1 e2) = If <$> deBruijnE b <*> deBruijnE e1 <*> deBruijnE e2
deBruijnE (AFT.Ap f x) = Ap <$> deBruijnE f <*> deBruijnE x
deBruijnE (AFT.Fun x e) = withPush x (Fun x <$> deBruijnE e)
deBruijnE (AFT.Fix f e) = withPush f (Fix f <$> deBruijnE e)
deBruijnE (AFT.Let x v e) = Let x <$> deBruijnE v <*> withPush x (deBruijnE e)

deBruijn :: AFT.Expr Name -> Expr
deBruijn e = evalState (deBruijnE e) []
