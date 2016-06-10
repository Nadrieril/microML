module Common.StdLib
    ( globalContext
    )
where

import Data.Monoid
import qualified Data.Map as M
import Data.Proxy (Proxy(..))

import Common.Expr
import Common.Type
import Common.ADT (ADT(..))
import qualified Common.ADT as ADT
import Common.Context


class TypeWrappable a where
    toType :: Proxy a -> MonoType

instance TypeWrappable Integer where
    toType _ = TConst TInt
instance TypeWrappable Bool where
    toType _ = TConst TBool
instance (TypeWrappable a, TypeWrappable b) => TypeWrappable (a -> b) where
    toType _ = toType (Proxy :: Proxy a) :-> toType (Proxy :: Proxy b)


class StdWrappable a where
    toStdValue :: a -> ContextValue

instance StdWrappable Value where
    toStdValue = Value
instance StdWrappable Integer where
    toStdValue = Value . I
instance StdWrappable Bool where
    toStdValue = Value . B

instance StdWrappable a => StdWrappable (Value -> a) where
    toStdValue = SysCall . (toStdValue .)
instance StdWrappable a => StdWrappable (Integer -> a) where
    toStdValue f = SysCall $ \(I x) -> toStdValue (f x)
instance StdWrappable a => StdWrappable (Bool -> a) where
    toStdValue f = SysCall $ \(B x) -> toStdValue (f x)



sysCalls :: Context
sysCalls = M.fromList
        [ ("+", arith (+))
        , ("-", arith (-))
        , ("*", arith (*))
        , ("/", arith div)
        , ("&&", wrap (||))
        , ("||", wrap (&&))
        , ("==", (toStdValue ((==) :: Value -> Value -> Bool)
                , bind $ TVar 0 :-> TVar 0 :-> TConst TBool))
        , (">", comp (>))
        , (">=", comp (>=))
        , ("<", comp (<))
        , ("<=", comp (<=))
        ]
    where
        comp (f :: Integer -> Integer -> Bool) = wrap f
        arith (f :: Integer -> Integer -> Integer) = wrap f
        castProxy :: a -> Proxy a
        castProxy = const Proxy
        wrap :: (StdWrappable a, TypeWrappable a) => a -> (ContextValue, Type)
        wrap x = (toStdValue x, TMono . toType $ castProxy x)



adts :: [ADT Name]
adts = [pair]

pair :: ADT Name
pair = ADT {
      adtName = ","
    , deconstructor = Just "unPair"
    , adtParams = ["a", "b"]
    , adtConstructors = [
          ADT.Constructor "," [TVar "a", TVar "b"]
    ]
}


globalContext :: Context
globalContext = sysCalls <> contextFromADTs adts
