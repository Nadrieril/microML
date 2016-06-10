{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternSynonyms, OverloadedStrings #-}
-- ASM = Abstract Functional Tree
module AFT.Expr
    ( Expr
    , AbstractExpr(..)
    , Scope(..)
    , fromAST
    ) where

import Data.Maybe (isJust)
import Control.Monad (mplus)

import Common.Expr
import Common.Type
import qualified AST.Expr as AST


data AbstractExpr v a =
      Var v
    | Const Value
    | Fun (Scope v a)
    | Fix (Scope v a)
    | Ap a a
    | Let a (Scope v a)
    | If a a a

data Scope v e = Scope v e

pattern SFun n e <- Fun (Scope n e) where
    SFun n e = Fun (Scope n e)
pattern SFix n e <- Fix (Scope n e) where
    SFix n e = Fix (Scope n e)
pattern SLet n v e <- Let v (Scope n e) where
    SLet n v e = Let v (Scope n e)

type LabelledExp l v = LFixP (AbstractExpr v) l

type TypedExpr v = LabelledExp (Maybe (Mono Name)) v
type Expr = TypedExpr Name


fromAST :: AST.Expr -> Expr
fromAST (LFixP t (AST.Wrap (LFixP t' e))) =
    if isJust t && isJust t'
        then error "multiple type annotations on the same expression"
        else fromAST $ LFixP (t `mplus` t') e
fromAST (LFixP t e) = LFixP t $
    case e of
        AST.Var v -> Var v
        AST.Const c -> Const c
        AST.Fun n e -> SFun n (fromAST e)
        AST.Ap f x -> Ap (fromAST f) (fromAST x)
        AST.If b e1 e2 -> If (fromAST b) (fromAST e1) (fromAST e2)
        AST.Negate e -> Ap (untyped $ Ap (untyped $ Var "-") (untyped $ Const $ I 0)) (fromAST e)
        AST.Infix o e1 e2 -> Ap (untyped $ Ap (untyped $ Var o) (fromAST e1)) (fromAST e2)
        AST.Let x v e -> SLet x (fromAST v) (fromAST e)
        AST.LetR f v e -> SLet f (untyped $ SFix f (fromAST v)) (fromAST e)
        AST.Wrap _ -> error "impossible"
    where untyped = LFixP Nothing
