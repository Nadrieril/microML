{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, PatternSynonyms, OverloadedStrings #-}
module Common.StdLib
    ( StdLibValue(..)
    , sysCalls
    , getSysCall
    , sysCallToValue
    , sysCallToType
    )
where

import Data.Proxy (Proxy(..))

import Common.Expr
import Common.Type


sysCalls :: [Name]
sysCalls = ["+", "-", "*", "/", "&&", "||", "=="]

getSysCall :: Name -> SysCall
getSysCall (Name x) = case x of
    "+" -> Plus
    "-" -> Minus
    "*" -> Mult
    "/" -> Div
    "&&" -> And
    "||" -> Or
    "==" -> Eq
    x -> error $ "unknown syscall : " ++ x


data StdLibValue = Fun (Value -> StdLibValue) | Val Value


class TypeWrappable a where
    toType :: Proxy a -> MonoType

instance TypeWrappable Integer where
    toType _ = TConst TInt
instance TypeWrappable Bool where
    toType _ = TConst TBool
instance (TypeWrappable a, TypeWrappable b) => TypeWrappable (a -> b) where
    toType _ = toType (Proxy :: Proxy a) :-> toType (Proxy :: Proxy b)


class StdWrappable a where
    toStdValue :: a -> StdLibValue

instance StdWrappable Value where
    toStdValue = Val
instance StdWrappable Integer where
    toStdValue = Val . I
instance StdWrappable Bool where
    toStdValue = Val . B

instance StdWrappable a => StdWrappable (Value -> a) where
    toStdValue = Fun . (toStdValue .)
instance StdWrappable a => StdWrappable (Integer -> a) where
    toStdValue f = Fun $ \(I x) -> toStdValue (f x)
instance StdWrappable a => StdWrappable (Bool -> a) where
    toStdValue f = Fun $ \(B x) -> toStdValue (f x)



sysCallToValue :: SysCall -> StdLibValue
sysCallToValue = fst . sysCallToValType
sysCallToType :: SysCall -> Type
sysCallToType = snd . sysCallToValType


sysCallToValType :: SysCall -> (StdLibValue, Type)
sysCallToValType sc = case sc of
        Plus -> wrap ((+) :: Integer -> Integer -> Integer)
        Minus -> wrap ((-) :: Integer -> Integer -> Integer)
        Mult -> wrap ((*) :: Integer -> Integer -> Integer)
        Div -> wrap (div :: Integer -> Integer -> Integer)
        And -> wrap (||)
        Or -> wrap (&&)
        Eq -> ( toStdValue ((==) :: Value -> Value -> Bool)
              , bind $ TVar 0 :-> TVar 0 :-> TConst TBool)
    where
        castProxy :: a -> Proxy a
        castProxy = const Proxy
        wrap :: (StdWrappable a, TypeWrappable a) => a -> (StdLibValue, Type)
        wrap x = (toStdValue x, TMono . toType $ castProxy x)
