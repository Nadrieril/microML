{-# LANGUAGE FlexibleInstances #-}
module Common.ADT where

import Data.List (intercalate)
import qualified Data.Map as M
-- import qualified Debug.Trace as T
import Text.Printf (printf)

import Common.Expr
import Common.Type
import Utils.PrettyPrint


data ADT a = ADT {
      adtName :: Name
    , adtParams :: [Name]
    , adtConstructors :: [Constructor a]
    } deriving (Functor)

data Constructor a = Constructor {
      constructorName :: Name
    , constructorParams :: [Mono a]
    } deriving (Functor)


instance PrettyPrint (ADT Id) where
    pprint = pprint . unBindTypeVars

instance PrettyPrint (ADT Name) where
    pprint (ADT name params constructors) =
        let paramString = concatMap ((' ':) . pprint) params in
        let constructorsString = intercalate " | " $ map pprint constructors in
        printf "data %s%s = %s" (pprint name) paramString constructorsString

instance PrettyPrint (Constructor Name) where
    pprint (Constructor n p) =
        let paramString = concatMap ((' ':) . pprint) p in
        printf "%s%s" (pprint n) paramString


bindTypeVars :: ADT Name -> ADT Id
bindTypeVars adt =
    let paramMap = M.fromList $ zip (adtParams adt) [0..] in
    fmap (paramMap M.!) adt

unBindTypeVars :: ADT Id -> ADT Name
unBindTypeVars adt = fmap (adtParams adt !!) adt


type FuncInfo = (Name, Type, Int)

constructorsInfo :: ADT Id -> [FuncInfo]
constructorsInfo (ADT name params constructors) =
    let paramIds = take (length params) [0..] in
    let adttype = TProduct name (TVar <$> paramIds) in
    let makeF t = foldr (:->) t . constructorParams in
    map (\c -> (constructorName c, bind $ makeF adttype c, length $ constructorParams c)) constructors
