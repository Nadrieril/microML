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


import Common.Expr (Name)
import Utils.PrettyPrint


infixr 4 :->

type TId = Int

pattern TConst n <- TProduct n [] where
        TConst n = TProduct n []

pattern TBool <- "Bool" where
        TBool = "Bool"
pattern TInt <- "Int" where
        TInt = "Int"

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


instance PrettyPrint (Mono TId) where
    pprint = pprint . calcVarName

instance PrettyPrint (Mono Name) where
    pprint (TConst t) = pprint t
    pprint (TVar v) = printf "%s" (pprint v)
    pprint t@(_ :-> _) = printf "(%s)" (intercalate " -> " $ map pprint $ foldarrow t)
        where
            foldarrow (t1 :-> t2) = t1:foldarrow t2
            foldarrow x = [x]
    pprint (TTuple t1 t2) = printf "(%s, %s)" (pprint t1) (pprint t2)
    pprint (TProduct n l) = printf "%s%s" (pprint n) (concatMap ((' ':) . pprint) l)

instance Show MonoType where
    show = let ?toplevel = False in pprint


calcVarName :: Mono TId -> Mono Name
calcVarName t =
    let varNames = (:[]) <$> ['a'..] in
    let freeVars = IS.toList $ free $ TMono t in
    let m = M.fromList $ zip freeVars varNames in
    fmap (m M.!) t


instance Hashable a => Hashable (Mono a)
instance Hashable a => Hashable (Poly a)


pattern TTuple x y <- (TProduct "," [x, y]) where
        TTuple x y = TProduct "," [x, y]
pattern x :-> y <- (TProduct "->" [x, y]) where
        x :-> y = TProduct "->" [x, y]


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
