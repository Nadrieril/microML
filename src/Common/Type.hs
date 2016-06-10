{-# LANGUAGE DeriveGeneric, PatternSynonyms, FlexibleInstances #-}
module Common.Type
    ( TId
    , pattern TConst
    , pattern TBool
    , pattern TInt
    , Mono(..)
    , Poly(..)
    , MonoType
    , Type
    , pattern TTuple
    , pattern (:->)
    , bind
    , free
    , mergeTypes
    ) where

import GHC.Generics (Generic)
import Data.List (intercalate)
import Data.Hashable (Hashable)
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Control.Monad (zipWithM)
import Text.Printf (printf)


import Common.Expr (Name(..))


infixr 4 :->

type TId = Int

pattern TConst n <- TProduct n [] where
        TConst n = TProduct n []

pattern TBool <- Name "Bool" where
        TBool = Name "Bool"
pattern TInt <- Name "Int" where
        TInt = Name "Int"

data Mono a =
      TVar a
    | TProduct Name [Mono a]
    deriving (Eq, Generic, Functor)

data Poly a =
      TMono (Mono a)
    | TBound a (Poly a)
    deriving (Eq, Generic, Functor)

type Type = Poly TId
type MonoType = Mono TId


instance Show (Mono TId) where
    show = show . calcVarName

instance Show (Mono Name) where
    show (TConst t) = show t
    show (TVar v) = printf "%s" (show v)
    show t@(_ :-> _) = printf "(%s)" (intercalate " -> " $ map show $ foldarrow t)
        where
            foldarrow (t1 :-> t2) = t1:foldarrow t2
            foldarrow x = [x]
    show (TTuple t1 t2) = printf "(%s, %s)" (show t1) (show t2)
    show (TProduct n l) = printf "%s%s" (show n) (concatMap ((' ':) . show) l)


calcVarName :: Mono TId -> Mono Name
calcVarName t =
    let varNames = Name . (:[]) <$> ['a'..] in
    let freeVars = IS.toList $ free $ TMono t in
    let m = M.fromList $ zip freeVars varNames in
    fmap (m M.!) t


instance Hashable a => Hashable (Mono a)
instance Hashable a => Hashable (Poly a)


pattern TTuple x y <- (TProduct (Name ",") [x, y]) where
        TTuple x y = TProduct (Name ",") [x, y]
pattern x :-> y <- (TProduct (Name "->") [x, y]) where
        x :-> y = TProduct (Name "->") [x, y]


free :: Type -> IS.IntSet
free (TBound i t) = IS.delete i (free t)
free (TMono t) = f t
    where
        f (TVar i) = IS.singleton i
        f (TProduct _ l) = IS.unions $ fmap f l

bind :: MonoType -> Type
bind t = IS.foldr TBound (TMono t) (free $ TMono t)

mergeTypes :: MonoType -> MonoType -> Either (MonoType, MonoType) MonoType
mergeTypes (TVar i1) (TVar i2) = return $ TVar (min i1 i2)
mergeTypes (TVar _) t2 = return t2
mergeTypes t1 (TVar _) = return t1
mergeTypes (TProduct n1 tl1) (TProduct n2 tl2)
        | n1 == n2 = TProduct n1 <$> zipWithM mergeTypes tl1 tl2
mergeTypes t1 t2 | t1 == t2 = return t1
mergeTypes t1 t2 = Left (t1, t2)
