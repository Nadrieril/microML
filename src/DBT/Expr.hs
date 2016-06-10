{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, PatternSynonyms #-}
-- DBT = De Bruijn Tree
module DBT.Expr
    ( Expr
    , AbstractExpr(..)
    , Scope(..)
    , Id
    , LabelledExp
    , TypedExpr
    , Program
    , afttodbt
    , pattern SFun, pattern SFix, pattern SLet
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (forM)
import Control.Monad.State (State, get, evalState)
import Control.Arrow (first)
import Text.Printf (printf)

import Utils (Stack, withPush, withPushAll)
import AST.Parse (isOperator)
import Common.Expr
import Common.Pattern
import Common.Type
import Common.Context
import qualified AFT.Expr as AFT

data AbstractExpr a =
      Bound Name Id
    | Free Name
    | Const Value
    | If a a a
    | Fun (Scope Name a)
    | Fix (Scope Name a)
    | Let a (Scope Name a)
    | Match a [Scope [Name] (Pattern BoundVar, a)]
    | Ap a a
    deriving (Functor, Foldable, Traversable)

data Scope v e = Scope v e
    deriving (Functor, Foldable, Traversable)

pattern SFun e <- Fun (Scope _ e)
pattern SFix e <- Fix (Scope _ e)
pattern SLet v e <- Let v (Scope _ e)

type LabelledExp l = LFixP AbstractExpr l

type Expr = LabelledExp (Maybe (Mono Name))
type TypedExpr = LabelledExp MonoType

type Program = (Context, TypedExpr)

instance PrettyPrint TypedExpr where
    pprint (LFixP _ e) = case e of
        Bound n _ -> showIdent n
        Free x -> showIdent x
        Const c -> pprint c
        Fun (Scope n e) -> printf "(\\%s -> %s)" (pprint n) (pprint e)
        Fix (Scope n e) -> printf "fix(\\%s -> %s)" (pprint n) (pprint e)
        Let v (Scope n e) ->
            let typeAnnotation = printf "/// %s :: %s\n" (showIdent n) (pprint $ label v) in
            let (isRec, v') = case v of { LFixP _ (Fix (Scope n' x)) | n == n' -> (True, x) ; _ -> (False, v) } in
            let rec = if isRec then "rec " else "" in
            let (params, v'') = unfoldFun v' in
            let paramStr = concatMap ((' ':) . pprint) params in
            let expr = printf "let %s%s%s = %s in\n%s" rec (showIdent n) paramStr (let ?toplevel = False in pprint v'') (pprint e) in
            (if ?toplevel then typeAnnotation else "") ++ expr
        Match e l -> let patterns :: String = concat [printf "\n| %s -> %s" (pprint p) (pprint e) | Scope _ (p, e) <- l]
            in printf "match %s with %s end" (pprint e) patterns
        Ap (expr -> Ap (expr -> Free o) x) y | isOperator o -> printf "(%s %s %s)" (pprint x) (pprint o) (pprint y)
        Ap f x@(expr -> Ap _ _) -> printf "%s (%s)" (pprint f) (pprint x)
        Ap f x -> printf "%s %s" (pprint f) (pprint x)
        If b e1 e2 -> printf "if %s then %s else %s" (pprint b) (pprint e1) (pprint e2)
        where
            showIdent n = if isOperator n then printf "(%s)" n else n
            unfoldFun (expr -> Fun (Scope n e)) = first (n:) $ unfoldFun e
            unfoldFun x = ([], x)

instance Show TypedExpr where
    show = let ?toplevel = False in pprint


deBruijn :: LabelledExp l -> LabelledExp l
deBruijn e = evalState (deBruijnS e) []
    where
        deBruijnS :: LabelledExp l -> State (Stack Name) (LabelledExp l)
        deBruijnS (LFixP t e) = LFixP t <$> case e of
            Const c -> return $ Const c
            Bound n i -> return $ Bound n i
            Free n -> do
                s <- get
                return $ case n `elemIndex` s of
                    Just i -> Bound n i
                    Nothing -> Free n
            If b e1 e2 -> If <$> deBruijnS b <*> deBruijnS e1 <*> deBruijnS e2
            Ap g x -> Ap <$> deBruijnS g <*> deBruijnS x
            Fun s -> Fun <$> auxScope s
            Fix s -> Fix <$> auxScope s
            Let v s -> Let <$> deBruijnS v <*> auxScope s
            Match e l -> Match <$> deBruijnS e <*> forM l auxPatScope
            where
                auxScope s@(Scope n _) = traverse (withPush n . deBruijnS) s
                auxPatScope s@(Scope n _) = traverse (traverse (withPushAll n . deBruijnS)) s



fromAFT :: AFT.Expr -> Expr
fromAFT (LFixP t e) = LFixP t $ case e of
    AFT.Var x -> Free x
    AFT.Const c -> Const c
    AFT.If b e1 e2 -> If (fromAFT b) (fromAFT e1) (fromAFT e2)
    AFT.Ap f x -> Ap (fromAFT f) (fromAFT x)
    AFT.Fun s -> Fun $ auxScope s
    AFT.Fix s -> Fix $ auxScope s
    AFT.Let v s -> Let (fromAFT v) (auxScope s)
    AFT.Match e l -> Match (fromAFT e)
        [ let binders = getPatternBinders p in
            Scope binders (fmap (\x -> BoundVar x (fromJust $ x `elemIndex` binders)) p, fromAFT e)
        | AFT.Scope p e <- l]
    where auxScope (AFT.Scope n e) = Scope n (fromAFT e)


afttodbt :: AFT.Expr -> Expr
afttodbt = deBruijn . fromAFT
