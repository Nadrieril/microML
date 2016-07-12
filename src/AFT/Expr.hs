{-# LANGUAGE PatternSynonyms #-}
-- AFT = Abstract Functional Tree
module AFT.Expr
    ( Expr
    , AbstractExpr(..)
    , Scope(..)
    , fromAST
    ) where

import Data.Maybe (isJust)
import Control.Monad (mplus)

import Common.Expr
import Common.Pattern
import Common.Type
import qualified Parse.AST as AST


data AbstractExpr a =
      Var Name
    | Const Value
    | Fun (Scope Name a)
    | Fix (Scope Name a)
    | Ap a a
    | Let a (Scope Name a)
    | Match a [(Pattern Name, Scope [Name] a)]
    | If a a a

data Scope v e = Scope v e

type LabelledExp l = LFixP AbstractExpr l

type Expr = LabelledExp (Maybe (Mono Name))


fromAST :: AST.Expr -> Expr
fromAST (LFixP t (AST.Wrap (LFixP t' e))) =
    if isJust t && isJust t'
        then error "multiple type annotations on the same expression"
        else fromAST $ LFixP (t `mplus` t') e
fromAST (LFixP t e) = LFixP t $
    case e of
        AST.Var v -> Var v
        AST.Const c -> Const c
        AST.Fun n e -> mkFun n (fromAST e)
        AST.Ap f x -> Ap (fromAST f) (fromAST x)
        AST.If b e1 e2 -> If (fromAST b) (fromAST e1) (fromAST e2)
        AST.Negate e -> Ap (untyped $ Ap (untyped $ Var "-") (untyped $ Const $ I 0)) (fromAST e)
        AST.Infix o e1 e2 -> Ap (untyped $ Ap (untyped $ Var o) (fromAST e1)) (fromAST e2)
        AST.Let x l v e -> mkLet x (fargs l $ fromAST v) (fromAST e)
        AST.LetR f l v e -> mkLet f (untyped $ mkFix f (fargs l $ fromAST v)) (fromAST e)
        AST.Match e l -> Match (fromAST e) [ (p, Scope (getPatternBinders p) (fromAST e)) | (p, e) <- l ]
        AST.Wrap _ -> error "impossible"
    where
        untyped = LFixP Nothing
        mkFun n e = Fun (Scope n e)
        mkFix n e = Fix (Scope n e)
        mkLet n v e = Let v (Scope n e)
        fargs :: [Name] -> Expr -> Expr
        fargs = flip $ foldr (\n e -> untyped $ mkFun n e)
