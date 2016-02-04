{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AFT.Expr
    ( Expr
    , AbstractExpr(..)
    , fromAST
    ) where

import Data.Maybe (isJust)
import Text.Printf (printf)

import Common.Expr
import Common.Type
import qualified AST.Expr as AST


data AbstractExpr v a =
      Var v
    | Const Value
    | Fun v a
    | Fix v a
    | Ap a a
    | Let v a a
    | If a a a

type LabelledExp l v = LFixP l (AbstractExpr v)

type TypedExpr v = LabelledExp (Maybe (Mono Name)) v
type UntypedExpr v = AbstractExpr v (TypedExpr v)
type Expr = TypedExpr Name

instance Show v => Show (TypedExpr v) where
    show (LFixP Nothing e) = showE e
    show (LFixP (Just t) e) = printf "%s :: %s" (showE e) (show t)

showE :: Show v => UntypedExpr v -> String
showE (Var x) = show x
showE (Const c) = show c
showE (Ap f x) = printf "(%s %s)" (show f) (show x)
showE (Fun x e) = printf "(\\%s -> %s)" (show x) (show e)
showE (Fix f e) = printf "fix(\\%s -> %s)" (show f) (show e)
showE (Let x v e) = printf "let %s = %s in\n%s" (show x) (show v) (show e)
showE (If b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe a b = case a of
    Nothing -> b
    Just a -> Just a

fromAST :: AST.Expr -> Expr
fromAST (LFixP t (AST.Wrap (LFixP t' e))) =
    if isJust t && isJust t'
        then error "multiple type annotations on the same expression"
        else fromAST $ LFixP (t `mergeMaybe` t') e
fromAST (LFixP t e) = LFixP t $
    case e of
        AST.Var v -> Var v
        AST.Const c -> Const c
        AST.Fun n e -> Fun n (fromAST e)
        AST.Ap f x -> Ap (fromAST f) (fromAST x)
        AST.If b e1 e2 -> If (fromAST b) (fromAST e1) (fromAST e2)
        AST.Infix o e1 e2 -> Ap (untyped $ Ap (untyped $ Var o) (fromAST e1)) (fromAST e2)
        AST.Let x v e -> Let x (fromAST v) (fromAST e)
        AST.LetR f v e -> Let f (untyped $ Fix f (fromAST v)) (fromAST e)
        AST.Wrap _ -> error "impossible"
    where untyped = LFixP Nothing
