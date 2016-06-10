{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Common.Expr where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)


class PrettyPrint a where
    pprint :: (?toplevel :: Bool) => a -> String

instance PrettyPrint String where
    pprint = id


type Id = Int

instance PrettyPrint Id where
    pprint = show


data BoundVar = BoundVar Name Id
    deriving (Eq, Ord, Generic, Hashable)

instance PrettyPrint BoundVar where
    pprint (BoundVar n _) = n


type Name = String


data Value = B Bool | I Integer
    deriving (Eq, Ord)

instance Show Value where
  show (B x) = show x
  show (I x) = show x

instance PrettyPrint Value where
    pprint = show

instance Read Value where
    readsPrec _ = readParen False
                  (\r -> [(B True, s) | ("True", s) <- lex r]
                      ++ [(B False, s) | ("False", s) <- lex r]
                      ++ [(I i, s) | (i, s) <- reads r])


data LFixP f l = LFixP { label :: l, expr :: f (LFixP f l) }
    deriving (Functor, Foldable, Traversable)
