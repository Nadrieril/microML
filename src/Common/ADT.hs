{-# LANGUAGE FlexibleInstances #-}
module Common.ADT where

import Data.Maybe (fromMaybe)
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
    , deconstructor :: Maybe Name
    } deriving (Functor)

data Constructor a = Constructor {
      constructorName :: Name
    , constructorParams :: [Mono a]
    } deriving (Functor)


instance PrettyPrint (ADT Id) where
    pprint = pprint . unBindTypeVars

instance PrettyPrint (ADT Name) where
    pprint (ADT name params constructors _) =
        let paramString = concatMap ((' ':) . pprint) params in
        let constructorsString = intercalate " | " $ map pprint constructors in
        printf "data %s%s = %s" (pprint name) paramString constructorsString

instance PrettyPrint (Constructor Name) where
    pprint (Constructor n p) =
        let paramString = concatMap ((' ':) . pprint) p in
        printf "%s%s" (pprint n) paramString


deconstructorName :: ADT a -> Name
deconstructorName ADT{..} = fromMaybe ("un" ++ adtName) deconstructor


bindTypeVars :: ADT Name -> ADT Id
bindTypeVars adt =
    let paramMap = M.fromList $ zip (adtParams adt) [0..] in
    fmap (paramMap M.!) adt

unBindTypeVars :: ADT Id -> ADT Name
unBindTypeVars adt = fmap (adtParams adt !!) adt


type FuncInfo = (Name, Type, Int)

deconstructorInfo :: ADT Id -> FuncInfo
deconstructorInfo adt@(ADT name params constructors _) =
    let retType = TVar (-1) in
    let paramIds = take (length params) [0..] in
    let adttype = TProduct name (TVar <$> paramIds) in
    let makeF t (Constructor _ p) = foldr (:->) t p in
    let deconstructorType = bind $ foldr ((:->) . makeF retType) (adttype :-> retType) constructors in
    (deconstructorName adt, deconstructorType, length constructors)

constructorsInfo :: ADT Id -> [FuncInfo]
constructorsInfo (ADT name params constructors _) =
    let paramIds = take (length params) [0..] in
    let adttype = TProduct name (TVar <$> paramIds) in
    let makeF t = foldr (:->) t . constructorParams in
    map (\c -> (constructorName c, bind $ makeF adttype c, length $ constructorParams c)) constructors
