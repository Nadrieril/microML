{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module DBT.Infer
    ( inferType
    , UnificationError
    ) where

import Data.Typeable (Typeable)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Control.Monad (zipWithM_, forM)
import Control.Arrow (first)
import Control.Eff (Member, Eff, run)
import Control.Eff.State.Strict (State, get, put, modify, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Control.Eff.Writer.Strict (Writer, tell, runWriter)
import Text.Printf (printf)
-- import qualified Debug.Trace as T

import Utils (Stack)
import qualified Utils (trace)
import qualified Utils.UnionFind as UF
import qualified DBT.Expr as DBT
import Common.Expr
import Common.Type
import qualified Common.Context as C
import Common.StdLib (globalContext)
import DBT.Expr

trace :: Show a => a -> b -> b
trace = Utils.trace False


getFree :: C.Context -> Name -> Type
getFree ctx x | Just (_, t) <- x `M.lookup` ctx = t
getFree _ x = error $ printf "Unknown free variable: %s" (show x)


data UnificationError = UnificationError (Maybe TypedExpr) MonoType MonoType

instance Show UnificationError where
  show (UnificationError e t1 t2) = let expstr = fromMaybe "?" (show <$> e) in
      printf "Cannot unify %s and %s in (%s)" (show t1) (show t2) expstr


type Env r e = (
    Member (Reader C.Context) r,
    Member (State Int) r,
    Member (State (Stack Type)) r,
    Member (State (UF.UnionFind MonoType)) r,
    Member (Writer UnificationError) r)
    => Eff r e


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
union x y = do
    uf <- get
    case UF.unionMergeFail mergeTypes x y uf of
        Right uf -> put uf
        Left (t1, t2) -> tell $ UnificationError Nothing t1 t2

find :: MonoType -> Env r MonoType
find t = state (UF.find t)

findDeep :: MonoType -> Env r MonoType
findDeep t = do
    t <- find t
    case t of
        TProduct n tl -> do
            t' <- TProduct n <$> mapM findDeep tl
            t `union` t'
            return t'
        _ -> return t

getType :: Id -> Env r Type
getType i = (!! i) <$> get

inst :: Type -> Env r MonoType
inst t = do
    let (l, t') = unbind t
    freshs <- forM l (const freshV)
    let tmap = M.fromList $ zip l freshs
    return $ fmap (\i -> fromMaybe i (M.lookup i tmap)) t'
    where
        unbind :: Type -> ([TId], MonoType)
        unbind (TMono t) = ([], t)
        unbind (TBound i t) = first (i:) $ unbind t


unify :: TypedExpr -> MonoType -> MonoType -> Env r ()
unify e t1 t2 = trace ("unify " ++ show t1 ++ ", " ++ show t2) $ do
    t1 <- find t1
    t2 <- find t2
    unify_ t1 t2
    where
        unify_ :: MonoType -> MonoType -> Env r ()
        unify_ t1@(TVar _) t2 = t1 `union` t2
        unify_ t1 t2@(TVar _) = t1 `union` t2
        unify_ (TProduct n1 tl1) (TProduct n2 tl2)
            | n1 == n2 = zipWithM_ (unify e) tl1 tl2
        unify_ t1 t2
            | t1 == t2 = return ()
            | otherwise = tell $ UnificationError (Just e) t1 t2


partialBind :: MonoType -> Env r Type
partialBind t = do
    stk <- get
    let freeInT = free $ TMono t
    let freeInStack = IS.unions (map free stk)
    return $ IS.foldr TBound (TMono t) (freeInT IS.\\ freeInStack)

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
            e@(LFixP t' e') <- infE
            t <- projectType t
            unify e t t'
            return $ LFixP t e'

    where infE = case e of
            Const c ->
                return $ LFixP (typeof c) (Const c)

            Bound x -> do
                t <- getType x
                s <- inst t
                return $ LFixP s (Bound x)

            Free x -> do
                ctx <- ask
                s <- inst $ getFree ctx x
                return $ LFixP s (Free x)

            If b e1 e2 -> do
                b <- inferTypeE b
                e1 <- inferTypeE e1
                e2 <- inferTypeE e2
                let e = LFixP (label e1) (If b e1 e2)
                unify e (label b) (TConst TBool)
                unify e (label e1) (label e2)
                return e

            Ap f x -> do
                f <- inferTypeE f
                x <- inferTypeE x
                t <- TVar <$> freshV
                let e = LFixP t (Ap f x)
                unify e (label f) (label x :-> t)
                return e

            Fun s -> do
                t <- TVar <$> freshV
                s@(Scope _ e) <- inferScope (TMono t) s
                return $ LFixP (t :-> label e) (Fun s)

            Fix s -> do
                t <- TVar <$> freshV
                s@(Scope _ e) <- inferScope (TMono t) s
                let e' = LFixP t (Fix s)
                unify e' t (label e)
                return e'

            Let v s -> do
                LFixP t v <- inferTypeE v
                t <- findDeep t
                t' <- partialBind t
                s@(Scope _ e) <- inferScope t' s
                return $ LFixP (label e) (Let (LFixP t v) s)

          inferScope :: Type -> Scope DBT.Expr -> Env r (Scope TypedExpr)
          inferScope t (Scope n e) = Scope n <$> localPush t (inferTypeE e)


inferType :: C.Context -> DBT.Expr -> ([UnificationError], TypedExpr)
inferType ctx e = run $
    flip runReader (globalContext <> ctx) $
    evalState (0 :: Int) $
    evalState ([] :: Stack Type) $
    evalState (UF.empty :: UF.UnionFind MonoType) $
    runWriter (:) ([] :: [UnificationError]) $ do
        -- LFixP t e <- inferTypeE e
        -- t <- find t
        -- let e' = LFixP t e
        -- trace uf $ return e'

        e <- inferTypeE e
        -- e <- traverse findDeep e
        (uf :: UF.UnionFind MonoType) <- get
        trace uf $ return e
