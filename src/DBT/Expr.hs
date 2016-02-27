{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, PatternSynonyms #-}
-- DBT = De Bruijn Tree
module DBT.Expr
    ( Expr
    , AbstractExpr(..)
    , Scope(..)
    , Id
    , LabelledExp
    , TypedExpr
    , afttodbt
    , pattern SFun , pattern SFix , pattern SLet
    ) where

import Text.Printf (printf)
import Data.List (elemIndex)
import Safe (atMay)
import Control.Monad.State (State, get, evalState)

import Utils (Stack, withPush)
import AST.Parse (isOperator)
import Common.Expr
import Common.Type
import qualified AFT.Expr as AFT

data AbstractExpr v a =
      Bound v
    | Free Name
    | Const Value
    | If a a a
    | Fun (Scope a)
    | Fix (Scope a)
    | Let a (Scope a)
    | Ap a a
    deriving (Functor)

data Scope e = Scope Name e
    deriving (Functor)

pattern SFun e <- Fun (Scope _ e)
pattern SFix e <- Fix (Scope _ e)
pattern SLet v e <- Let v (Scope _ e)

type LabelledExp l v = LFixP l (AbstractExpr v)

type Expr = LabelledExp (Maybe (Mono Name)) Id
type TypedExpr = LabelledExp MonoType Id


mapBind :: (Stack Name -> Either Name a -> Either Name a) -> LabelledExp l a -> LabelledExp l a
mapBind f e = evalState (mapBind' f e) []
    where
        mapBind' :: (Stack Name -> Either Name a -> Either Name a) -> LabelledExp l a -> State (Stack Name) (LabelledExp l a)
        mapBind' f (LFixP t e) = LFixP t <$> case e of
            Bound i -> get >>= auxf (Right i)
            Free n -> get >>= auxf (Left n)
            Const c -> return $ Const c
            If b e1 e2 -> If <$> mapBind' f b <*> mapBind' f e1 <*> mapBind' f e2
            Ap g x -> Ap <$> mapBind' f g <*> mapBind' f x
            Fun s -> Fun <$> auxScope s
            Fix s -> Fix <$> auxScope s
            Let v s -> Let <$> mapBind' f v <*> auxScope s
            where
                auxScope (Scope n e) = Scope n <$> withPush n (mapBind' f e)
                auxf x s = return $ case f s x of
                        Right i -> Bound i
                        Left n -> Free n

instance Show Expr where
    show (unDebruijn -> LFixP t e) = case t of
            Nothing -> s
            Just t -> printf "%s :: %s" s (show t)
        where s = case e of
                Bound x -> show x
                Free x -> show x
                Const c -> show c
                Ap (expr -> Ap (expr -> Free o) x) y | isOperator o -> printf "(%s %s %s)" (show x) (show o) (show y)
                Ap f x -> printf "(%s %s)" (show f) (show x)
                Let v (Scope x e) -> printf "let %s = %s in\n%s" (show x) (show v) (show e)
                If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
                Fun (Scope x e) -> printf "(\\%s -> %s)" (show x) (show e)
                Fix (Scope x e) -> printf "fix(\\%s -> %s)" (show x) (show e)

instance Show TypedExpr where
    show (unDebruijn -> LFixP _ e) = case e of
        Bound i -> printf "#%d" i
        Free x -> showIdent x
        Const c -> show c
        Fun (Scope n e) -> printf "(\\%s -> %s)" (show n) (show e)
        Fix (Scope n e) -> printf "fix(\\%s -> %s)" (show n) (show e)
        Let v (Scope n e) -> let (params, v') = unfoldFun v in
            let paramStr = concatMap ((' ':) . show) params in
            printf "/// %s :: %s\nlet %s%s = %s in\n%s" (showIdent n) (show $ label v) (showIdent n) paramStr (show $ untype v') (show e)
        Ap (expr -> Ap (expr -> Free o) x) y | isOperator o -> printf "(%s %s %s)" (show x) (show o) (show y)
        Ap f x@(expr -> Ap _ _) -> printf "%s (%s)" (show f) (show x)
        Ap f x -> printf "%s %s" (show f) (show x)
        If b e1 e2 -> printf "if %s then %s else %s" (show b) (show e1) (show e2)
        where
            showIdent n = (if isOperator n then printf "(%s)" else id) (show n)
            unfoldFun (expr -> Fun (Scope n e)) =
                let (l, e') = unfoldFun e in
                (n:l, e')
            unfoldFun x = ([], x)
            untype :: TypedExpr -> Expr
            untype (LFixP _ e) = LFixP Nothing $ fmap untype e


unDebruijn :: LabelledExp l Id -> LabelledExp l Id
unDebruijn = mapBind $ \s -> \case
    Right i -> case s `atMay` i of
        Just n -> Left n
        Nothing -> Right i
    Left n -> Left n

deBruijn :: LabelledExp l Id -> LabelledExp l Id
deBruijn = mapBind $ \s -> \case
    Right i -> Right i
    Left n -> case n `elemIndex` s of
                Just i -> Right i
                Nothing -> Left n

fromAFT :: AFT.Expr -> Expr
fromAFT (LFixP t e) = LFixP t $ case e of
    AFT.Var x -> Free x
    AFT.Const c -> Const c
    AFT.If b e1 e2 -> If (fromAFT b) (fromAFT e1) (fromAFT e2)
    AFT.Ap f x -> Ap (fromAFT f) (fromAFT x)
    AFT.Fun s -> Fun $ auxScope s
    AFT.Fix s -> Fix $ auxScope s
    AFT.Let v s -> Let (fromAFT v) (auxScope s)
    where auxScope (AFT.Scope n e) = Scope n (fromAFT e)


afttodbt :: AFT.Expr -> Expr
afttodbt = deBruijn . fromAFT
