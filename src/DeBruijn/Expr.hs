{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, PatternSynonyms, MultiParamTypeClasses, FunctionalDependencies #-}
module DeBruijn.Expr
    ( Name(..)
    , Value(..)
    , Expr
    , AExpr(..)
    , Id
    , LFixP (..)
    , LExp
    , deBruijn
    , calcVarName
    , pattern AVar, pattern AGlobal, pattern AConst, pattern AIf
    , pattern AFun, pattern AFix, pattern ALet, pattern AAp
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Control.Monad.State (State, get, gets, evalState)

import Utils (Stack, withPush, local, push)
import AFT.Expr (Name, Value)
import qualified AFT.Expr as AFT

data AExpr v a =
      Var v
    | Global Name
    | Const Value
    | If a a a
    | Fun Name a
    | Fix Name a
    | Let Name a a
    | Ap a a

data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }

type LExp l v = LFixP l (AExpr v)



class AST v a | a -> v where
  proj :: a -> AExpr v a
  inj :: AExpr v a -> a

pattern AVar v <- (proj -> Var v) where
        AVar v = inj (Var v)
pattern AGlobal v <- (proj -> Global v) where
        AGlobal v = inj (Global v)
pattern AConst v <- (proj -> Const v) where
        AConst v = inj (Const v)
pattern AIf a1 a2 a3 <- (proj -> If a1 a2 a3) where
        AIf a1 a2 a3 = inj (If a1 a2 a3)
pattern AFun v a <- (proj -> Fun v a) where
        AFun v a = inj (Fun v a)
pattern AFix v a <- (proj -> Fix v a) where
        AFix v a = inj (Fix v a)
pattern ALet v a1 a2 <- (proj -> Let v a1 a2) where
        ALet v a1 a2 = inj (Let v a1 a2)
pattern AAp a1 a2 <- (proj -> Ap a1 a2) where
        AAp a1 a2 = inj (Ap a1 a2)


instance AST v (LExp l v) where
  proj (LFixP _ e) = e
  inj = LFixP undefined


type Id = Int

type Expr = LExp () Id

instance Show Expr where
  show =  show . calcVarName

instance Show (LExp () Name) where
    show (AVar i) = show i
    show (AGlobal x) = show x
    show (AConst c) = show c
    show (AFun n e) = printf "(\\%s -> %s)" (show n) (show e)
    show (AFix n e) = printf "fix(\\%s -> %s)" (show n) (show e)
    show (ALet n v e) = printf "let %s = %s in\n%s" (show n) (show v) (show e)
    show (AAp f x) = printf "(%s %s)" (show f) (show x)
    show (AIf b e1 e2) = printf "if %s then %s else %s" (show b) (show e1) (show e2)
    show _ = error "impossible"


calcVarName :: LExp l Id -> LExp l Name
calcVarName e = evalState (calcVarName'' e) []

calcVarName'' :: LExp l Id -> State (Stack Name) (LExp l Name)
calcVarName'' (LFixP l e) = LFixP l <$> calcVarName' e

calcVarName' :: AExpr Id (LExp l Id) -> State (Stack Name) (AExpr Name (LExp l Name))
calcVarName' (Var i) = Var <$> gets (!! i)
calcVarName' (Global x) = return $ Global x
calcVarName' (Const c) = return $ Const c
calcVarName' (Fun n e) = local $ do
        push n
        Fun n <$> calcVarName'' e
calcVarName' (Fix n e) = local $ do
        push n
        Fix n <$> calcVarName'' e
calcVarName' (Let n v e) = local $ do
        push n
        Let n <$> calcVarName'' v <*> calcVarName'' e
calcVarName' (Ap f x) = Ap <$> calcVarName'' f <*> calcVarName'' x
calcVarName' (If b e1 e2) = If <$> calcVarName'' b <*> calcVarName'' e1 <*> calcVarName'' e2




deBruijnAE :: AFT.Expr Name -> State (Stack Name) Expr
deBruijnAE = fmap (LFixP ()) . deBruijnE

deBruijnE :: AFT.Expr Name -> State (Stack Name) (AExpr Id Expr)
deBruijnE (AFT.Var x) = do
    s <- get
    return $ case elemIndex x s of
        Just i -> Var i
        Nothing -> Global x
deBruijnE (AFT.Const c) = return $ Const c
deBruijnE (AFT.If b e1 e2) = If <$> deBruijnAE b <*> deBruijnAE e1 <*> deBruijnAE e2
deBruijnE (AFT.Ap f x) = Ap <$> deBruijnAE f <*> deBruijnAE x
deBruijnE (AFT.Fun x e) = withPush x (Fun x <$> deBruijnAE e)
deBruijnE (AFT.Fix f e) = withPush f (Fix f <$> deBruijnAE e)
deBruijnE (AFT.Let x v e) = Let x <$> deBruijnAE v <*> withPush x (deBruijnAE e)

deBruijn :: AFT.Expr Name -> Expr
deBruijn e = evalState (deBruijnAE e) []
