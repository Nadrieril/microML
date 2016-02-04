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
    show (LFixP _ e) = case e of
        Var i -> show i
        Global x -> show x
        Const c -> show c
        Fun n e -> printf "(\\%s -> %s)" (show n) (show e)
        Fix n e -> printf "fix(\\%s -> %s)" (show n) (show e)
        Let n v e -> printf "let %s :: %s = %s in\n%s" (show n) (show $ label v) (show v) (show e)
        Ap f x -> printf "(%s %s)" (show f) (show x)
        If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
