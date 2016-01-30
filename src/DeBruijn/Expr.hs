module DeBruijn.Expr
    ( Name(..)
    , Value(..)
    , Expr(..)
    , Id
    , deBruijn
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Data.Char (chr, ord)
import Control.Monad.State (State, get, gets, modify, evalState)

import Utils (Stack, withPush, local)
import AFT.Expr (Name, Value)
import qualified AFT.Expr as AFT


type Id = Int

data Expr =
      Var Id
    | Global Name
    | Const Value
    | Fun Expr
    | Fix Expr
    | Ap Expr Expr
    | Let Expr Expr
    | If Expr Expr Expr

instance Show Expr where
  show e =  evalState (showE e) (-1)

showVar :: Int -> String
showVar i = [chr (i + ord 'a')]

showE :: Expr -> State Int String
showE (Var i) = showVar <$> gets (\j -> j-i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun e) = local $ do
        modify (+ 1)
        printf "(\\%s -> %s)" <$> gets showVar <*> showE e
showE (Fix e) = local $ do
        modify (+ 1)
        printf "fix(\\%s -> %s)" <$> gets showVar <*> showE e
showE (Let v e) = local $ do
        modify (+ 1)
        printf "let %s = %s in\n%s" <$> gets showVar <*> showE v <*> showE e
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
deBruijnE (AFT.Fun x e) = withPush x (Fun <$> deBruijnE e)
deBruijnE (AFT.Fix f e) = withPush f (Fix <$> deBruijnE e)
deBruijnE (AFT.Let x v e) = Let <$> deBruijnE v <*> withPush x (deBruijnE e)

deBruijn :: AFT.Expr Name -> Expr
deBruijn e = evalState (deBruijnE e) []
