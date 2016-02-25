{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Common.ADT where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Map as M
-- import qualified Debug.Trace as T
import Text.Printf (printf)

import Common.Expr
import Common.Type

type Id = Int

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

instance Show (ADT Id) where
    show = show . unBindTypeVars

instance Show (ADT Name) where
    show (ADT name params constructors _) =
        let paramString = concatMap ((' ':) . show) params in
        let constructorsString = intercalate " | " $ map show constructors in
        printf "data %s%s = %s" (show name) paramString constructorsString

instance Show (Constructor Name) where
    show (Constructor n p) =
        let paramString = concatMap ((' ':) . show) p in
        printf "%s%s" (show n) paramString


deconstructorName :: ADT a -> Name
deconstructorName ADT{..} = fromMaybe (Name ("un" ++ show adtName)) deconstructor


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


adts :: [ADT Id]
adts = map bindTypeVars [pair, option, list]


pair :: ADT Name
pair = ADT {
      adtName = ","
    , deconstructor = Just "unPair"
    , adtParams = ["a", "b"]
    , adtConstructors = [
          Constructor "," [TVar "a", TVar "b"]
    ]
}

option :: ADT Name
option = ADT {
      adtName = "Option"
    , deconstructor = Nothing
    , adtParams = ["a"]
    , adtConstructors = [
          Constructor "None" []
        , Constructor "Some" [TVar "a"]
    ]
}

list :: ADT Name
list = ADT {
      adtName = "List"
    , deconstructor = Nothing
    , adtParams = ["a"]
    , adtConstructors = [
          Constructor "Nil" []
        , Constructor "Cons" [TVar "a", TProduct "List" [TVar "a"]]
    ]
}
