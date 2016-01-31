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


typeE :: DeBruijn.Expr -> Env r Expr
typeE (DeBruijn.AConst c) = return $ LFixP (Mono $ TConst $ typeof c) (Const c)

typeE (DeBruijn.AVar x) = do
    t <- getType x
    s <- inst t
    return $ LFixP (Mono s) (Var x)

typeE (DeBruijn.AGlobal x) = do
    let t = stdLibTypes M.! x
    s <- inst t
    return $ LFixP (Mono s) (Global x)

typeE (DeBruijn.AIf b e1 e2) = do
    b@(LFixP (Mono tb) _) <- typeE b
    unify tb (TConst TBool)
    e1@(LFixP (Mono t1) _) <- typeE e1
    e2@(LFixP (Mono t2) _) <- typeE e2
    unify t1 t2
    return $ LFixP (Mono t1) (If b e1 e2)

typeE (DeBruijn.AAp f x) = do
    f@(LFixP (Mono tf) _) <- typeE f
    x@(LFixP (Mono tx) _) <- typeE x
    t <- freshV
    unify tf (tx :-> t)
    return $ LFixP (Mono t) (Ap f x)

typeE (DeBruijn.AFun n e) = do
    t <- freshV
    e@(LFixP (Mono t') _) <-
        localEnv $ do
            push (Mono t)
            typeE e
    return $ LFixP (Mono $ t :-> t') (Fun n e)

typeE (DeBruijn.AFix n e) = do
    t <- freshV
    e'@(LFixP (Mono te) _) <- localEnv $ do
        push (Mono $ t :-> t)
        typeE e
    unify (t :-> t) te
    return $ LFixP (Mono (t :-> t)) (Fix n e')
    -- TODO: replace t :-> t with t

typeE (DeBruijn.ALet n v e) = do
    LFixP t v <- typeE v
    t <- findP t
    t' <- bind t
    e@(LFixP t'' _) <-
        localEnv $ do
            push t'
            typeE e
    return $ LFixP t'' (Let n (LFixP t v) e)

typeE _ = error "impossible"


inferType :: DeBruijn.Expr -> Expr
inferType e = run $
    evalState (0 :: Int) $
    evalState ([] :: Stack Type) $
    evalState (UF.empty :: UF.UnionFind MonoType) $ do
        LFixP t e <- typeE e
        t <- bind t
        let e' = LFixP t e
        (uf :: UF.UnionFind MonoType) <- get
        -- trace uf $ traverse findP e'
        trace uf $ return e'
