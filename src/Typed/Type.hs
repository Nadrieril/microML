{-# LANGUAGE DeriveGeneric, DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Typed.Type
    ( TId
    , TConst(..)
    , MonoType(..)
    , PolyType(..)
    , Type
    , free
    , mergeTypes
    , mapVars
    ) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import qualified Data.IntSet as IS
import Text.Printf (printf)

infixr 4 :->

type TId = Int

data TConst = TBool | TInt
    deriving (Eq, Generic)

data MonoType =
      TConst TConst
    | MonoType :-> MonoType
    | TVar TId
    deriving (Eq, Generic)

type Type = PolyType MonoType

data PolyType a =
      Mono a
    | Bound TId (PolyType a)
    deriving (Eq, Generic, Functor)


instance Show TConst where
    show TBool = "Bool"
    show TInt = "Int"

instance Show MonoType where
    show (TConst t) = show t
    show (TVar i) = printf "#%d" i
    show (t1 :-> t2) = printf "(%s -> %s)" (show t1) (show t2)

instance Show Type where
    show (Mono t) = show t
    show (Bound i t) = printf "\\#%d.%s" i (show t)


instance Hashable TConst
instance Hashable MonoType
instance Hashable Type


free :: Type -> IS.IntSet
free (Bound i t) = IS.delete i (free t)
free (Mono t) = f t
    where
        f (TConst _) = IS.empty
        f (TVar i) = IS.singleton i
        f (t1 :-> t2) = IS.union (f t1) (f t2)


mergeTypes :: MonoType -> MonoType -> MonoType
mergeTypes (TVar i1) (TVar i2) = TVar (min i1 i2)
mergeTypes (TVar _) t2 = t2
mergeTypes t1 (TVar _) = t1
mergeTypes t1 t2 = error $ printf "Cannot merge different types (%s and %s)" (show t1) (show t2)

mapVars :: (TId -> TId) -> Type -> Type
mapVars f (Bound i t) = Bound (f i) (mapVars f t)
mapVars f (Mono t) = Mono $ go t
    where go (TVar i) = TVar $ f i
          go (t1 :-> t2) = go t1 :-> go t2
          go t = t
