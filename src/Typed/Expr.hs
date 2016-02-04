{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Typed.Expr
    ( VId
    , Name(..)
    , Value(..)
    , AbstractExpr(..)
    , Expr
    , LFixP(..)
    ) where

import Text.Printf (printf)

import Common.Expr
import Common.Type (MonoType)
import DeBruijn.Expr hiding (Expr)

type VId = Int

type Expr = LabelledExp MonoType VId


instance Show Expr where
    show e@(LFixP t _) = printf "%s\n:: %s" (show $ calcVarName e) (show t)

instance Show (LabelledExp MonoType Name) where
    show (AVar i) = show i
    show (AGlobal x) = show x
    show (AConst c) = show c
    show (AFun n e) = printf "(\\%s -> %s)" (show n) (show e)
    show (AFix n e) = printf "fix(\\%s -> %s)" (show n) (show e)
    show (ALet n v e) = printf "let %s :: %s = %s in\n%s" (show n) (show $ label v) (show v) (show e)
    show (AAp f x) = printf "(%s %s)" (show f) (show x)
    show (AIf b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
    show _ = error "impossible"
