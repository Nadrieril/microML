module Common.Expr where

newtype Name = Name String
    deriving (Eq, Ord)

instance Show Name where
  show (Name o) = o


data Value = B Bool | I Integer | Tuple (Value, Value)
    deriving (Eq, Ord)

instance Show Value where
  show (B x) = show x
  show (I x) = show x
  show (Tuple x) = show x


data SysCall =
      Plus | Minus | Mult | Div
    | And | Or | Eq
    | Fst | Snd | Pair
    deriving (Show)


data LFixP l f = LFixP { label :: l, expr :: f (LFixP l f) }
