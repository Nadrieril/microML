{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Common.StdLib where

import qualified Data.Map as M
import Data.Proxy (Proxy(..))
import Text.Printf (printf)
import Control.Arrow ((&&&))

import Common.Expr
import Common.Type (TConst(..), Mono(..), Poly(..), MonoType, Type)


getSysCall :: Name -> SysCall
getSysCall (Name x) = case x of
    "+" -> Plus
    "-" -> Minus
    "*" -> Mult
    "/" -> Div
    "and" -> And
    "or" -> Or
    "==" -> Eq
    x -> error $ "unknown syscall : " ++ x


data StdLibValue = Fun (Value -> StdLibValue) | Val Value


class StdWrappable a where
    toType :: Proxy a -> MonoType
    toValue :: a -> StdLibValue

instance StdWrappable Value where
    toType _ = undefined
    toValue = Val
instance StdWrappable Integer where
    toType _ = TConst TInt
    toValue = Val . I
instance StdWrappable Bool where
    toType _ = TConst TBool
    toValue = Val . B

instance StdWrappable a => StdWrappable (Value -> a) where
    toType _ = undefined
    toValue = Fun . (toValue .)
instance StdWrappable a => StdWrappable (Integer -> a) where
    toType _ = TConst TInt :-> toType (Proxy :: Proxy a)
    toValue f = Fun $ \(I x) -> toValue (f x)
instance StdWrappable a => StdWrappable (Bool -> a) where
    toType _ = TConst TBool :-> toType (Proxy :: Proxy a)
    toValue f = Fun $ \(B x) -> toValue (f x)


stdLib :: M.Map Name (Value -> Value -> Value)
stdLib = fmap binf stdLib'
    where
      binf :: StdLibValue -> Value -> Value -> Value
      binf stdv x y =
          let Fun f = stdv
          in let Fun g = f x
          in let Val v = g y
          in v

stdLib' :: M.Map Name StdLibValue
stdLib' = fmap fst stdLibWithTypes

stdLibTypes :: M.Map Name Type
stdLibTypes = fmap snd stdLibWithTypes

stdLibWithTypes :: M.Map Name (StdLibValue, Type)
stdLibWithTypes = M.fromList $
    fmap ((id &&& sysCallToValue . getSysCall) . Name)
    ["+", "-", "*", "/", "or", "and", "=="]

sysCallToValue :: SysCall -> (StdLibValue, Type)
sysCallToValue sc = case sc of
        Plus -> wrap ((+) :: Integer -> Integer -> Integer)
        Minus -> wrap ((-) :: Integer -> Integer -> Integer)
        Mult -> wrap ((*) :: Integer -> Integer -> Integer)
        Div -> wrap (div :: Integer -> Integer -> Integer)
        And -> wrap (||)
        Or -> wrap (&&)
        Eq -> (toValue eq, Bound 0 (Mono (TVar 0 :-> TVar 0 :-> TConst TBool)))
    where
        castProxy :: a -> Proxy a
        castProxy = const Proxy
        wrap :: StdWrappable a => a -> (StdLibValue, Type)
        wrap x = (toValue x, Mono . toType $ castProxy x)

        eq (I x) (I y) = B (x==y)
        eq (B x) (B y) = B (x==y)
        eq x y = error $ printf "Error: %s and %s cannot be compared" (show x) (show y)
