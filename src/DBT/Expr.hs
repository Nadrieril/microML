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
    , fromAFT
    , pattern SFun, pattern SFix, pattern SLet
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (forM)
import Control.Monad.State (State, get, evalState)
import Control.Arrow (first)
import Text.Printf (printf)

import Utils (Stack, withPush, withPushAll)
import Utils.PrettyPrint
import Parse.Token (isOperator)
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
    | Fun (Scope a)
    | Fix (Scope a)
    | Let a (Scope a)
    | Match a [(Pattern BoundVar, Scope a)]
    | Ap a a
    deriving (Functor, Foldable, Traversable)

data Scope e = Scope [Name] e
    deriving (Functor, Foldable, Traversable)

pattern SFun e <- Fun (Scope _ e)
pattern SFix e <- Fix (Scope _ e)
pattern SLet v e <- Let v (Scope _ e)
pattern Var n <- (\case { Free n -> Just n;  Bound n _ -> Just n; _ -> Nothing } -> Just n)

type LabelledExp l = LFixP AbstractExpr l

type Expr = LabelledExp (Maybe (Mono Name))
type TypedExpr = LabelledExp MonoType

type Program = (Context, TypedExpr)

instance PrettyPrint TypedExpr where
    pprint (LFixP _ e) = case e of
        Bound n _ -> showIdent n
        Free x -> showIdent x
        Const c -> pprint c
        Fun (Scope ns e) -> printf "(\\%s -> %s)" (pprint $ head ns) (pprint e)
        Fix (Scope ns e) -> printf "fix(\\%s -> %s)" (pprint $ head ns) (pprint e)
        Let v (Scope ns e) ->
            let n = head ns in
            let typeAnnotation = printf "/// %s :: %s\n" (showIdent n) (pprint $ label v) in
            let (isRec, v') = case v of { LFixP _ (Fix (Scope ns' x)) | ns == ns' -> (True, x) ; _ -> (False, v) } in
            let rec = if isRec then "rec " else "" in
            let (params, v'') = unfoldFun v' in
            let paramStr = concatMap ((' ':) . pprint) params in
            let expr = printf "let %s%s%s = %s in\n%s" rec (showIdent n) paramStr (let ?toplevel = False in pprint v'') (pprint e) in
            (if ?toplevel then typeAnnotation else "") ++ expr
        Match e l -> let patterns :: String = concat [printf "\n| %s -> %s" (pprint p) (pprint e) | (p, Scope _ e) <- l]
            in printf "match %s with %s end" (pprint e) patterns
        Ap (expr -> Ap (expr -> Var o) x) y | isOperator o -> printf "(%s %s %s)" (pprint x) (pprint o) (pprint y)
        Ap f x@(expr -> Ap _ _) -> printf "%s (%s)" (pprint f) (pprint x)
        Ap f x -> printf "%s %s" (pprint f) (pprint x)
        If b e1 e2 -> printf "if %s then %s else %s" (pprint b) (pprint e1) (pprint e2)
        where
            showIdent n = if isOperator n then printf "(%s)" n else n
            unfoldFun (expr -> Fun (Scope ns e)) = first (ns++) $ unfoldFun e
            unfoldFun x = ([], x)

instance Show TypedExpr where
    show = let ?toplevel = False in pprint


fromAFT :: AFT.Expr -> Expr
fromAFT e = evalState (aux e) []
    where
        aux :: AFT.Expr -> State (Stack Name) Expr
        aux (LFixP t e) = LFixP t <$> case e of
            AFT.Const c -> return $ Const c
            AFT.Var n -> do
                s <- get
                return $ case n `elemIndex` s of
                    Just i -> Bound n i
                    Nothing -> Free n
            AFT.If b e1 e2 -> If <$> aux b <*> aux e1 <*> aux e2
            AFT.Ap f x -> Ap <$> aux f <*> aux x
            AFT.Fun s -> Fun <$> auxScope s
            AFT.Fix s -> Fix <$> auxScope s
            AFT.Let v s -> Let <$> aux v <*> auxScope s
            AFT.Match e l -> Match <$> aux e <*> forM l auxPatScope
            where
                auxScope (AFT.Scope n e) = traverse (withPush n . aux) (Scope [n] e)
                auxPatScope (p, AFT.Scope ns e) =
                    let p' = fmap (\x -> BoundVar x (fromJust $ x `elemIndex` ns)) p in
                    traverse (traverse (withPushAll ns . aux)) (p', Scope ns e)
