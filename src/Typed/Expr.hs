{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Typed.Expr
    ( Name(..)
    , Value(..)
    , Expr(..)
    , SExpr(..)
    , inferType
    ) where

import Text.Printf (printf)
import Data.Char (chr, ord)
import qualified Data.Map as M ((!))
import qualified Data.IntSet as IS
import Control.Monad.State.Strict (State, get, gets, put, modify, evalState, runState)

import Utils (Stack, local, trace)
import DeBruijn.Expr (Name, Value(..))
import qualified DeBruijn.Expr as DeBruijn
import StdLib (stdLibTypes)
import qualified Typed.UnionFind as UF
import Typed.Type

type VId = Int

data Expr a = TExpr a (SExpr a)
    deriving (Functor, Foldable, Traversable)

data SExpr a =
      Var VId
    | Global Name
    | Const Value
    | Fun (Expr a)
    | Fix (Expr a)
    | Ap (Expr a) (Expr a)
    | Let (Expr a) (Expr a)
    | If (Expr a) (Expr a) (Expr a)
    deriving (Functor, Foldable, Traversable)


instance Show a => Show (Expr a) where
  show e@(TExpr t _) = printf "%s\n:: %s" (evalState (showTE e) (-1)) (show t)

showVar :: Int -> String
showVar i = [chr (i + ord 'a')]

showTE :: Show a => Expr a -> State Int String
showTE (TExpr _ e) = showE e

showE :: Show a => SExpr a -> State Int String
showE (Var i) = showVar <$> gets (\j -> j-i)
showE (Global x) = return $ show x
showE (Const c) = return $ show c
showE (Fun e) = local $ do
        modify (+ 1)
        printf "(\\%s -> %s)" <$> gets showVar <*> showTE e
showE (Fix e) = local $ do
        modify (+ 1)
        printf "fix(\\%s -> %s)" <$> gets showVar <*> showTE e
showE (Let v e) = local $ do
        modify (+ 1)
        let TExpr t _ = v
        printf "let %s :: %s = %s in\n%s" <$> gets showVar <*> pure (show t) <*> showTE v <*> showTE e
showE (Ap f x) = printf "(%s %s)" <$> showTE f <*> showTE x
showE (If b e1 e2) = printf "if %s then %s else %s" <$> showTE b <*> showTE e1 <*> showTE e2



type Env a = State (Int, Stack Type, UF.UnionFind MonoType) a

localEnv :: Env a -> Env a
localEnv m = do
    (_, stk, _) <- get
    ret <- m
    modify (\(i, _, uf) -> (i, stk, uf))
    return ret


freshV :: Env MonoType
freshV = do
    (i, stk, uf) <- get
    put (i+1, stk, uf)
    return $ TVar i

union :: MonoType -> MonoType -> Env ()
union x y = do
    (i, stk, uf) <- get
    let uf' = UF.union' uf mergeTypes x y
    put (i, stk, uf')

find' :: MonoType -> Env MonoType
find' x = do
    (i, stk, uf) <- get
    let (y, uf') = UF.find uf x
    put (i, stk, uf')
    return y

find :: MonoType -> Env MonoType
find (t1 :-> t2) = (:->) <$> find t1 <*> find t2
find t = find' t

findP :: Type -> Env Type
findP (Mono t) = Mono <$> find t
findP (Bound i t) = do
    TVar i <- find (TVar i)
    Bound i <$> findP t


getType :: VId -> Env Type
getType i = do
    (_, stk, _) <- get
    return $ stk !! i

push :: Type -> Env ()
push t = do
    (i, stk, uf) <- get
    put (i, t:stk, uf)

inst :: Type -> Env MonoType
inst (Mono t) = return t
inst (Bound i t) = do
    TVar i' <- freshV
    inst $ mapVars (\j -> if i==j then i' else j) t

unify :: MonoType -> MonoType -> Env ()
unify t1 t2 = trace ("unify " ++ show t1 ++ ", " ++ show t2) $ do
    t1 <- find t1
    t2 <- find t2
    unify_ t1 t2
    where
        unify_ :: MonoType -> MonoType -> Env ()
        unify_ (t11 :-> t12) (t21 :-> t22) = unify_ t11 t21 >> unify_ t12 t22
        unify_ t1@(TVar _) t2 = t1 `union` t2
        unify_ t1 t2@(TVar _) = t1 `union` t2
        unify_ t1 t2 | t1 == t2 = return ()
                     | otherwise = error $ printf "Cannot unify %s and %s" (show t1) (show t2)


bind :: Type -> Env Type
bind t = do
    t <- findP t
    let freeInT = free t
    (_, stk, _) <- get
    let freeInStack = IS.unions (map free stk)
    return $ IS.foldr Bound t (freeInT IS.\\ freeInStack)

typeof :: Value -> TConst
typeof (B _) = TBool
typeof (I _) = TInt

typeE :: DeBruijn.Expr -> Env (Expr Type)
typeE (DeBruijn.Const c) = return $ TExpr (Mono $ TConst $ typeof c) (Const c)

typeE (DeBruijn.Var x) = do
    t <- getType x
    s <- inst t
    return $ TExpr (Mono s) (Var x)

typeE (DeBruijn.Global x) =
    let t = stdLibTypes M.! x in
    return $ TExpr t (Global x)

typeE (DeBruijn.If b e1 e2) = do
    b@(TExpr (Mono tb) _) <- typeE b
    unify tb (TConst TBool)
    e1@(TExpr (Mono t1) _) <- typeE e1
    e2@(TExpr (Mono t2) _) <- typeE e2
    unify t1 t2
    return $ TExpr (Mono t1) (If b e1 e2)

typeE (DeBruijn.Ap f x) = do
    f@(TExpr (Mono tf) _) <- typeE f
    x@(TExpr (Mono tx) _) <- typeE x
    t <- freshV
    unify tf (tx :-> t)
    return $ TExpr (Mono t) (Ap f x)

typeE (DeBruijn.Fun e) = do
    t <- freshV
    e@(TExpr (Mono t') _) <-
        localEnv $ do
            push (Mono t)
            typeE e
    return $ TExpr (Mono $ t :-> t') (Fun e)

typeE (DeBruijn.Fix _) = error "Cannot type recursive definition yet"

typeE (DeBruijn.Let v e) = do
    TExpr t v <- typeE v
    t <- findP t
    t' <- bind t
    e@(TExpr t'' _) <-
        localEnv $ do
            push t'
            typeE e
    return $ TExpr t'' (Let (TExpr t v) e)

inferType :: DeBruijn.Expr -> Expr Type
inferType e = flip evalState (0, [], UF.empty) $ do
        TExpr t e <- typeE e
        t <- bind t
        let e' = TExpr t e
        (_, _, uf) <- get
        -- trace uf $ traverse findP e'
        trace uf $ return e'
