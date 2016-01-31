module StdLib where

import qualified Data.Map as M
import Text.Printf (printf)
import Control.Arrow (first)

import AST.Expr (Name(..), Value(..))
import Typed.Type (TConst(..), Mono(..), Poly(..), Type)

stdLib :: M.Map Name (Value -> Value -> Value)
stdLib = M.fromList $ fmap (first Name) [
      ("*", withInt (*)),
      ("/", withInt div),
      ("+", withInt (+)),
      ("-", withInt (-)),
      ("or", withBool (||)),
      ("and", withBool (&&)),
      ("==", eq)
    ]
    where
      withInt :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
      withInt f (I x) (I y) = I $ f x y
      withInt _ v _ = error $ printf "Error: attempting to evaluate %s as int" (show v)

      withBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
      withBool f (B x) (B y) = B $ f x y
      withBool _ v _ = error $ printf "Error: attempting to evaluate %s as bool" (show v)

      eq (I x) (I y) = B (x==y)
      eq (B x) (B y) = B (x==y)
      eq x y = error $ printf "Error: %s and %s cannot be compared" (show x) (show y)

stdLibTypes :: M.Map Name Type
stdLibTypes = M.fromList $ fmap (first Name) [
      ("*", intOp),
      ("/", intOp),
      ("+", intOp),
      ("-", intOp),
      ("or", boolOp),
      ("and", boolOp),
      ("==", Bound 0 (Mono (TVar 0 :-> TVar 0 :-> TConst TBool)))
    ]
    where
      intOp = Mono $ TConst TInt :-> TConst TInt :-> TConst TInt
      boolOp = Mono $ TConst TBool :-> TConst TBool :-> TConst TBool
