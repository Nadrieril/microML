{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}
module Common.Expr where

import Data.Hashable (Hashable)
import Text.Printf (printf)


newtype Name = Name String
    deriving (Eq, Ord, Hashable)

instance Show Name where
  show (Name o) = o


data Value = B Bool | I Integer | Product Name [Value]
    deriving (Eq, Ord)

instance Show Value where
  show (B x) = show x
  show (I x) = show x
  show (Tuple x y) = printf "(%s, %s)" (show x) (show y)
  show (Product n l) = printf "%s%s" (show n) (show l)

pattern Tuple x y <- (Product (Name ",") [x, y]) where
        Tuple x y = Product (Name ",") [x, y]

data SysCall =
      Plus | Minus | Mult | Div
    | And | Or | Eq
    | Fst | Snd | Pair
    deriving (Show)


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }
