-- ASM = Abstract Stack Machine
module ASM.Instr where

import Common.Expr (Value(..))

type Id = Int

data BinOp = Plus | Minus | Mult | Div | And | Or | Eq
    deriving (Show)

data Instr =
      Access Id
    | Apply
    | Cur [Instr]
    | Return
    | Let
    | Endlet
    | Branchneg Id
    | Branch Id
    | Op BinOp
    | Push Value
    deriving (Show)
