{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns, MultiParamTypeClasses #-}
module Typed.Expr
    ( VId
    , Name(..)
    , Value(..)
    , AExpr(..)
    , Expr
    , LFixP(..)
    ) where

import Text.Printf (printf)

import DeBruijn.Expr hiding (Expr)
import Typed.Type (Type)

type VId = Int

type Expr = LExp Type VId


instance Show Expr where
    show e@(LFixP t _) = printf "%s\n:: %s" (show $ calcVarName e) (show t)

instance Show (LExp Type Name) where
    show (AVar i) = show i
    show (AGlobal x) = show x
    show (AConst c) = show c
    show (AFun n e) = printf "(\\%s -> %s)" (show n) (show e)
    show (AFix n e) = printf "fix(\\%s -> %s)" (show n) (show e)
    show (ALet n v e) =
        let LFixP t _ = v in
        printf "let %s :: %s = %s in\n%s" (show n) (show t) (show v) (show e)
    show (AAp f x) = printf "(%s %s)" (show f) (show x)
    show (AIf b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
    show _ = error "impossible"
