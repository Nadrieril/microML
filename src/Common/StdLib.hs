{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, PatternSynonyms #-}
module Common.StdLib
    ( StdLibValue(..)
    , getSysCall
    , sysCallToValue
    , sysCallToType
    )
where

import Data.Proxy (Proxy(..))

import Common.Expr
import Common.Type (TConst(..), Mono(..), Poly(..), MonoType, Type, pattern TTuple)


getSysCall :: Name -> SysCall
getSysCall (Name x) = case x of
    "+" -> Plus
    "-" -> Minus
    "*" -> Mult
    "/" -> Div
    "and" -> And
    "or" -> Or
    "==" -> Eq
    "fst" -> Fst
    "snd" -> Snd
    "," -> Pair
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
instance (TypeWrappable a, TypeWrappable b) => TypeWrappable (a, b) where
    toType _ = TTuple (toType (Proxy :: Proxy a)) (toType (Proxy :: Proxy b))


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
instance StdWrappable a => StdWrappable ((Value, Value) -> a) where
    toStdValue f = Fun $ \(Tuple x y) -> toStdValue (f (x, y))
instance StdWrappable a => StdWrappable (Name -> [Value] -> a) where
    toStdValue f = Fun $ \(Product n l) -> toStdValue (f n l)



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
              , Bound 0 (Mono (TVar 0 :-> TVar 0 :-> TConst TBool)))
        Fst -> ( toStdValue (fst :: (Value, Value) -> Value)
               , Bound 0 $ Bound 1 $ Mono $ TTuple (TVar 0) (TVar 1) :-> TVar 0)
        Snd -> ( toStdValue (snd :: (Value, Value) -> Value)
               , Bound 0 $ Bound 1 $ Mono $ TTuple (TVar 0) (TVar 1) :-> TVar 1)
        Pair -> ( toStdValue Tuple
                , Bound 0 $ Bound 1 $ Mono $ TVar 0 :-> TVar 1 :-> TTuple (TVar 0) (TVar 1))
    where
        castProxy :: a -> Proxy a
        castProxy = const Proxy
        wrap :: (StdWrappable a, TypeWrappable a) => a -> (StdLibValue, Type)
        wrap x = (toStdValue x, Mono . toType $ castProxy x)
