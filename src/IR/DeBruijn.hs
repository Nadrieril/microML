module IR.DeBruijn where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, put, get, modify, evalState)

import IR.AFT (Name, Value)
import qualified IR.AFT as AFT

type Id = Int

data Expr =
      Var Id
    | Global Name
    | Const Value
    | Fun Expr
    | Fix Expr
    | Ap Expr Expr
    | If Expr Expr Expr

instance Show Expr where
  show (Var i) = printf "'%d" i
  show (Global x) = show x
  show (Const c) = show c
  show (Fun e) = printf "(\\%s)" (show e)
  show (Fix e) = printf "fix(\\%s)" (show e)
  show (Ap f x) = printf "(%s %s)" (show f) (show x)
  show (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)

type Stack = [Name]

push :: a -> State [a] ()
push x = modify (x:)

pop :: State [a] a
pop = do
    (x:q) <- get
    put q
    return x

withPush :: a -> State [a] b -> State [a] b
withPush x m = push x >> m >>= ((pop >>) . return)

deBruijnE :: AFT.Expr Name -> State Stack Expr
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

deBruijn :: AFT.Expr Name -> Expr
deBruijn e = evalState (deBruijnE e) []
