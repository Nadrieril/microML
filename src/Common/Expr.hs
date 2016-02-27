{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}
module Common.Expr where

import Data.Hashable (Hashable)
import Data.String (IsString(..))
import Control.Arrow (first)


type Id = Int

newtype Name = Name String
    deriving (Eq, Ord, Hashable)

instance Show Name where
  show (Name o) = o

instance IsString Name where
  fromString = Name

instance Read Name where
    readsPrec _ r = first Name <$> readParen False lex r


data Value = B Bool | I Integer
    deriving (Eq, Ord)

instance Show Value where
  show (B x) = show x
  show (I x) = show x

instance Read Value where
    readsPrec _ = readParen False
                  (\r -> [(B True, s) | ("True", s) <- lex r]
                      ++ [(B False, s) | ("False", s) <- lex r]
                      ++ [(I i, s) | (i, s) <- reads r])


data SysCall =
      Plus | Minus | Mult | Div
    | And | Or | Eq
    deriving (Show)


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }
