{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}
module Common.Expr where

import Data.Hashable (Hashable)
import Data.String (IsString(..))


newtype Name = Name String
    deriving (Eq, Ord, Hashable)

instance Show Name where
  show (Name o) = o

instance IsString Name where
  fromString = Name


data Value = B Bool | I Integer
    deriving (Eq, Ord)

instance Show Value where
  show (B x) = show x
  show (I x) = show x

data SysCall =
      Plus | Minus | Mult | Div
    | And | Or | Eq
    deriving (Show)


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }
