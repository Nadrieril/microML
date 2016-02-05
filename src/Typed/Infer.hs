{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables, LambdaCase #-}
module Typed.Infer
    ( inferType
    ) where

import Data.Typeable (Typeable)
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


state :: (Typeable s, Member (State s) r) => (s -> (a, s)) -> Eff r a
state f = do
    (x, s') <- f <$> get
    put s'
    return x

localPush :: Type -> Env r a -> Env r a
localPush x m = do
    modify (x:)
    ret <- m
    modify (tail :: Stack Type -> Stack Type)
    return ret


freshV :: Env r MonoType
freshV = do
    i <- get
    put (i+1)
    return $ TVar i

union :: MonoType -> MonoType -> Env r ()
union x y = modify (UF.union' mergeTypes x y)

find :: MonoType -> Env r MonoType
find (t1 :-> t2) = (:->) <$> find t1 <*> find t2
find t = do
    t <- state (UF.find t)
    case t of
        (_ :-> _) -> do -- We can unify further both sides
            t' <- find t
            t `union` t'
            return t'
        _ -> return t

getType :: VId -> Env r Type
getType i = (!! i) <$> get

inst :: Type -> Env r MonoType
inst (Mono t) = return t
inst (Bound i t) = do
    TVar i' <- freshV
    inst $ fmap (\j -> if i==j then i' else j) t

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


bind :: MonoType -> Env r Type
bind t = do
    t <- Mono <$> find t
    let freeInT = free t
    stk <- get
    let freeInStack = IS.unions (map free stk)
    return $ IS.foldr Bound t (freeInT IS.\\ freeInStack)

typeof :: Value -> TConst
typeof (B _) = TBool
typeof (I _) = TInt


typeE :: DeBruijn.Expr -> Env r Expr
typeE (DeBruijn.AConst c) =
    return $ LFixP (TConst $ typeof c) (Const c)

typeE (DeBruijn.AVar x) = do
    t <- getType x
    s <- inst t
    return $ LFixP s (Var x)

typeE (DeBruijn.AGlobal x) = do
    let t = stdLibTypes M.! x
    s <- inst t
    return $ LFixP s (Global x)

typeE (DeBruijn.AIf b e1 e2) = do
    b@(LFixP tb _) <- typeE b
    unify tb (TConst TBool)
    e1@(LFixP t1 _) <- typeE e1
    e2@(LFixP t2 _) <- typeE e2
    unify t1 t2
    return $ LFixP t1 (If b e1 e2)

typeE (DeBruijn.AAp f x) = do
    f@(LFixP tf _) <- typeE f
    x@(LFixP tx _) <- typeE x
    t <- freshV
    unify tf (tx :-> t)
    return $ LFixP t (Ap f x)

typeE (DeBruijn.AFun n e) = do
    t <- freshV
    e <- localPush (Mono t) $ typeE e
    return $ LFixP (t :-> label e) (Fun n e)

typeE (DeBruijn.AFix n e) = do
    t <- freshV
    e <- localPush (Mono t) $ typeE e
    unify t (label e)
    return $ LFixP t (Fix n e)

typeE (DeBruijn.ALet n v e) = do
    LFixP t v <- typeE v
    t <- find t
    t' <- bind t
    e <- localPush t' $ typeE e
    return $ LFixP (label e) (Let n (LFixP t v) e)

typeE _ = error "impossible"


inferType :: DeBruijn.Expr -> Expr
inferType e = run $
    evalState (0 :: Int) $
    evalState ([] :: Stack Type) $
    evalState (UF.empty :: UF.UnionFind MonoType) $ do
        LFixP t e <- typeE e
        t <- find t
        let e' = LFixP t e

        (uf :: UF.UnionFind MonoType) <- get
        -- trace uf $ traverse findP e'
        trace uf $ return e'