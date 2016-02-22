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
import Common.ADT
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
        Nothing -> infE
        Just t -> do
            e'' <- infE
            t <- projectType t
            unify t (label e'')
            return e''

    where infE = case e of
            Const c ->
                return $ LFixP (typeof c) (Const c)

            Var x -> do
                t <- getType x
                s <- inst t
                return $ LFixP s (Var x)

            Global x -> do
                let t = if x `M.member` adtFunMap
                        then adtFunMap M.! x
                        else StdLib.sysCallToType $ StdLib.getSysCall x
                s <- inst t
                return $ LFixP s (Global x)

            If b e1 e2 -> do
                b <- inferTypeE b
                unify (label b) (TConst TBool)
                e1 <- inferTypeE e1
                e2 <- inferTypeE e2
                unify (label e1) (label e2)
                return $ LFixP (label e1) (If b e1 e2)

            Ap f x -> do
                f <- inferTypeE f
                x <- inferTypeE x
                t <- TVar <$> freshV
                unify (label f) (label x :-> t)
                return $ LFixP t (Ap f x)

            Fun s -> do
                t <- TVar <$> freshV
                s@(Scope _ e) <- inferScope (Mono t) s
                return $ LFixP (t :-> label e) (Fun s)

            Fix s -> do
                t <- TVar <$> freshV
                s@(Scope _ e) <- inferScope (Mono t) s
                unify t (label e)
                return $ LFixP t (Fix s)

            Let v s -> do
                LFixP t v <- inferTypeE v
                t <- find t
                t' <- partialBind t
                s@(Scope _ e) <- inferScope t' s
                return $ LFixP (label e) (Let (LFixP t v) s)

          inferScope :: Type -> Scope DBT.Expr -> Env r (Scope TypedExpr)
          inferScope t (Scope n e) = Scope n <$> localPush t (inferTypeE e)


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
