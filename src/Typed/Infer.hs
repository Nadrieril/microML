{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}
module Typed.Infer
    ( inferType
    ) where

import Text.Printf (printf)
import qualified Data.Map as M ((!))
import qualified Data.IntSet as IS
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)

import Utils (Stack, trace)
import qualified DeBruijn.Expr as DeBruijn
import StdLib (stdLibTypes)
import qualified Typed.UnionFind as UF
import Typed.Type
import Typed.Expr

type Env r e = (Member (State Int) r, Member (State (Stack Type)) r, Member (State (UF.UnionFind MonoType)) r) => Eff r e

localEnv :: Env r a -> Env r a
localEnv m = do
    (stk :: Stack Type) <- get
    ret <- m
    put stk
    return ret

freshV :: Env r MonoType
freshV = do
    i <- get
    put (i+1)
    return $ TVar i

union :: MonoType -> MonoType -> Env r ()
union x y = modify (UF.union' mergeTypes x y)

find' :: MonoType -> Env r MonoType
find' x = do
    uf <- get
    let (y, uf') = UF.find uf x
    put uf'
    return y

find :: MonoType -> Env r MonoType
find (t1 :-> t2) = (:->) <$> find t1 <*> find t2
find t = find' t

findP :: Type -> Env r Type
findP (Mono t) = Mono <$> find t
findP (Bound i t) =
    find (TVar i) >>= \case
        TVar i' -> Bound i' <$> findP t
        _ -> findP t


getType :: VId -> Env r Type
getType i = (!! i) <$> get

push :: Type -> Env r ()
push t = modify (t:)

inst :: Type -> Env r MonoType
inst (Mono t) = return t
inst (Bound i t) = do
    TVar i' <- freshV
    inst $ mapVars (\j -> if i==j then i' else j) t

unify :: MonoType -> MonoType -> Env r ()
unify t1 t2 = trace ("unify " ++ show t1 ++ ", " ++ show t2) $ do
    t1 <- find t1
    t2 <- find t2
    unify_ t1 t2
    where
        unify_ :: MonoType -> MonoType -> Env r ()
        unify_ (t11 :-> t12) (t21 :-> t22) = unify_ t11 t21 >> unify_ t12 t22
        unify_ t1@(TVar _) t2 = t1 `union` t2
        unify_ t1 t2@(TVar _) = t1 `union` t2
        unify_ t1 t2 | t1 == t2 = return ()
                     | otherwise = error $ printf "Cannot unify %s and %s" (show t1) (show t2)


bind :: Type -> Env r Type
bind t = do
    t <- findP t
    let freeInT = free t
    stk <- get
    let freeInStack = IS.unions (map free stk)
    return $ IS.foldr Bound t (freeInT IS.\\ freeInStack)

typeof :: Value -> TConst
typeof (B _) = TBool
typeof (I _) = TInt


typeAE :: DeBruijn.Expr -> Env r (Expr Type)
typeAE = typeE . DeBruijn.runIdentity

typeE :: DeBruijn.AbsExpr DeBruijn.Identity -> Env r (Expr Type)
typeE (DeBruijn.Const c) = return $ TExpr (Mono $ TConst $ typeof c) (Const c)

typeE (DeBruijn.Var x) = do
    t <- getType x
    s <- inst t
    return $ TExpr (Mono s) (Var x)

typeE (DeBruijn.Global x) = do
    let t = stdLibTypes M.! x
    s <- inst t
    return $ TExpr (Mono s) (Global x)

typeE (DeBruijn.If b e1 e2) = do
    b@(TExpr (Mono tb) _) <- typeAE b
    unify tb (TConst TBool)
    e1@(TExpr (Mono t1) _) <- typeAE e1
    e2@(TExpr (Mono t2) _) <- typeAE e2
    unify t1 t2
    return $ TExpr (Mono t1) (If b e1 e2)

typeE (DeBruijn.Ap f x) = do
    f@(TExpr (Mono tf) _) <- typeAE f
    x@(TExpr (Mono tx) _) <- typeAE x
    t <- freshV
    unify tf (tx :-> t)
    return $ TExpr (Mono t) (Ap f x)

typeE (DeBruijn.Fun n e) = do
    t <- freshV
    e@(TExpr (Mono t') _) <-
        localEnv $ do
            push (Mono t)
            typeAE e
    return $ TExpr (Mono $ t :-> t') (Fun n e)

typeE (DeBruijn.Fix n e) = do
    t <- freshV
    e'@(TExpr (Mono te) _) <- localEnv $ do
        push (Mono $ t :-> t)
        typeAE e
    unify (t :-> t) te
    return $ TExpr (Mono (t :-> t)) (Fix n e')

typeE (DeBruijn.Let n v e) = do
    TExpr t v <- typeAE v
    t <- findP t
    t' <- bind t
    e@(TExpr t'' _) <-
        localEnv $ do
            push t'
            typeAE e
    return $ TExpr t'' (Let n (TExpr t v) e)

inferType :: DeBruijn.Expr -> Expr Type
inferType e = run $
    evalState (0 :: Int) $
    evalState ([] :: Stack Type) $
    evalState (UF.empty :: UF.UnionFind MonoType) $ do
        TExpr t e <- typeAE e
        t <- bind t
        let e' = TExpr t e
        (uf :: UF.UnionFind MonoType) <- get
        -- trace uf $ traverse findP e'
        trace uf $ return e'
