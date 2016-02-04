module Common.Expr where

newtype Name = Name String
    deriving (Eq, Ord)

instance Show Name where
  show (Name o) = o


data Value = B Bool | I Integer

instance Show Value where
  show (B x) = show x
  show (I x) = show x


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }
