{-# LANGUAGE DeriveGeneric, PatternSynonyms #-}
module Common.Type
    ( TId
    , TConst(..)
    , Mono(..)
    , Poly(..)
    , MonoType
    , Type
    , pattern TTuple
    , free
    , mergeTypes
    ) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.IntSet as IS
import Text.Printf (printf)

import Common.Expr (Name(..))

infixr 4 :->

type TId = Int

data TConst = TBool | TInt
    deriving (Eq, Generic)

data Mono a =
      TConst TConst
    | TVar a
    | Mono a :-> Mono a
    | TProduct Name [Mono a]
    deriving (Eq, Generic, Functor)

data Poly a =
      Mono (Mono a)
    | Bound a (Poly a)
    deriving (Eq, Generic, Functor)

type Type = Poly TId
type MonoType = Mono TId


instance Show TConst where
    show TBool = "Bool"
    show TInt = "Int"

instance Show a => Show (Mono a) where
    show (TConst t) = show t
    show (TVar v) = printf "#%s" (show v)
    show (t1 :-> t2) = printf "(%s -> %s)" (show t1) (show t2)
    show (TTuple t1 t2) = printf "(%s, %s)" (show t1) (show t2)
    show (TProduct n l) = printf "%s%s" (show n) (show l)

instance Show a => Show (Poly a) where
    show (Mono t) = show t
    show (Bound v t) = printf "\\#%s.%s" (show v) (show t)


instance Hashable TConst
instance Hashable a => Hashable (Mono a)
instance Hashable a => Hashable (Poly a)


pattern TTuple x y <- (TProduct (Name ",") [x, y]) where
        TTuple x y = TProduct (Name ",") [x, y]


free :: Type -> IS.IntSet
free (Bound i t) = IS.delete i (free t)
free (Mono t) = f t
    where
        f (TConst _) = IS.empty
        f (TVar i) = IS.singleton i
        f (t1 :-> t2) = IS.union (f t1) (f t2)
        f (TProduct _ l) = IS.unions $ fmap f l


mergeTypes :: MonoType -> MonoType -> MonoType
mergeTypes (TVar i1) (TVar i2) = TVar (min i1 i2)
mergeTypes (TVar _) t2 = t2
mergeTypes t1 (TVar _) = t1
mergeTypes (t11 :-> t12) (t21 :-> t22) = mergeTypes t11 t21 :-> mergeTypes t12 t22
mergeTypes (TProduct n1 tl1) (TProduct n2 tl2)
    | n1 == n2 = TProduct n1 $ zipWith mergeTypes tl1 tl2
    | otherwise = error $ printf "Cannot merge different types (%s and %s)" (show n1) (show n2)
mergeTypes t1 t2 | t1 == t2 = t1
                 | otherwise = error $ printf "Cannot merge different types (%s and %s)" (show t1) (show t2)
