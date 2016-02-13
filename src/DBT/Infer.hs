{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module DBT.Infer
    ( inferType
    ) where

import Data.Typeable (Typeable)
import Text.Printf (printf)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Control.Monad (zipWithM_)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)

import Utils (Stack)
import qualified Utils (trace)
import qualified Utils.UnionFind as UF
import qualified DBT.Expr as DBT
import qualified Common.StdLib as StdLib
import Common.Expr
import Common.Type
import DBT.Expr

trace :: Show a => a -> b -> b
trace = Utils.trace False

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


freshV :: Env r TId
freshV = do
    i <- get
    put (i+1)
    return i

union :: MonoType -> MonoType -> Env r ()
union x y = modify (UF.union' mergeTypes x y)

find :: MonoType -> Env r MonoType
find (TProduct n tl) = TProduct n <$> mapM find tl
find t = do
    t <- state (UF.find t)
    case t of
        (TProduct _ _) -> autoUnion t
        _ -> return t
    where
        autoUnion :: MonoType -> Env r MonoType
        autoUnion t = do -- We can unify further both sides
            t' <- find t
            t `union` t'
            return t'

getType :: Id -> Env r Type
getType i = (!! i) <$> get

inst :: Type -> Env r MonoType
inst (Mono t) = return t
inst (Bound i t) = do
    i' <- freshV
    inst $ fmap (\j -> if i==j then i' else j) t

unify :: MonoType -> MonoType -> Env r ()
unify t1 t2 = trace ("unify " ++ show t1 ++ ", " ++ show t2) $ do
    t1 <- find t1
    t2 <- find t2
    unify_ t1 t2
    where
        unify_ :: MonoType -> MonoType -> Env r ()
        unify_ t1@(TVar _) t2 = t1 `union` t2
        unify_ t1 t2@(TVar _) = t1 `union` t2
        unify_ (TProduct n1 tl1) (TProduct n2 tl2)
            | n1 == n2 = zipWithM_ unify_ tl1 tl2
            | otherwise = error $ printf "Cannot unify %s and %s" (show n1) (show n2)
        unify_ t1 t2 | t1 == t2 = return ()
                     | otherwise = error $ printf "Cannot unify %s and %s" (show t1) (show t2)


partialBind :: MonoType -> Env r Type
partialBind t = do
    t <- Mono <$> find t
    let freeInT = free t
    stk <- get
    let freeInStack = IS.unions (map free stk)
    return $ IS.foldr Bound t (freeInT IS.\\ freeInStack)

typeof :: Value -> Mono a
typeof (B _) = TConst TBool
typeof (I _) = TConst TInt

projectType :: Mono Name -> Env r MonoType
projectType t = evalState (M.empty :: M.Map Name Int) (f t)
    where
        f :: (Member (State (M.Map Name Int)) r) => Mono Name -> Env r MonoType
        f (TConst c) = return $ TConst c
        f (TVar n) = do
            state <- get
            case M.lookup n state of
                Just i -> return $ TVar i
                Nothing -> do
                    i <- freshV
                    put (M.insert n i state)
                    return $ TVar i
        f (TProduct n tl) = TProduct n <$> mapM f tl

inferTypeE :: DBT.Expr -> Env r TypedExpr
inferTypeE (LFixP t e) =
    case t of
        Nothing -> e'
        Just t -> do
            LFixP t' e'' <- e'
            t <- projectType t
            unify t t'
            return $ LFixP t' e''

    where e' = case e of
            DBT.Const c ->
                return $ LFixP (typeof c) (Const c)

            DBT.Var x -> do
                t <- getType x
                s <- inst t
                return $ LFixP s (Var x)

            DBT.Global x -> do
                let t = StdLib.sysCallToType $ StdLib.getSysCall x
                s <- inst t
                return $ LFixP s (Global x)

            DBT.If b e1 e2 -> do
                b@(LFixP tb _) <- inferTypeE b
                unify tb (TConst TBool)
                e1@(LFixP t1 _) <- inferTypeE e1
                e2@(LFixP t2 _) <- inferTypeE e2
                unify t1 t2
                return $ LFixP t1 (If b e1 e2)

            DBT.Ap f x -> do
                f@(LFixP tf _) <- inferTypeE f
                x@(LFixP tx _) <- inferTypeE x
                t <- TVar <$> freshV
                unify tf (tx :-> t)
                return $ LFixP t (Ap f x)

            DBT.Fun n e -> do
                t <- TVar <$> freshV
                e <- localPush (Mono t) $ inferTypeE e
                return $ LFixP (t :-> label e) (Fun n e)

            DBT.Fix n e -> do
                t <- TVar <$> freshV
                e <- localPush (Mono t) $ inferTypeE e
                unify t (label e)
                return $ LFixP t (Fix n e)

            DBT.Let n v e -> do
                LFixP t v <- inferTypeE v
                t <- find t
                t' <- partialBind t
                e <- localPush t' $ inferTypeE e
                return $ LFixP (label e) (Let n (LFixP t v) e)


inferType :: DBT.Expr -> TypedExpr
inferType e = run $
    evalState (0 :: Int) $
    evalState ([] :: Stack Type) $
    evalState (UF.empty :: UF.UnionFind MonoType) $ do
        LFixP t e <- inferTypeE e
        t <- find t
        let e' = LFixP t e

        (uf :: UF.UnionFind MonoType) <- get
        -- trace uf $ traverse findP e'
        trace uf $ return e'
