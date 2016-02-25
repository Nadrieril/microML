{-# LANGUAGE OverloadedStrings #-}
module Common.ADT where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
-- import qualified Debug.Trace as T

import Common.Expr
import Common.Type

type Id = Int

data ADT a = ADT {
      adtName :: Name
    , adtParams :: [a]
    , adtConstructors :: [Constructor a]
    , deconstructor :: Maybe Name
    }

data Constructor a = Constructor {
      constructorName :: Name
    , constructorParams :: [Mono a]
    }


deconstructorName :: ADT a -> Name
deconstructorName ADT{..} = fromMaybe (Name ("un" ++ show adtName)) deconstructor


bindTypeVars :: ADT Name -> ADT Id
bindTypeVars (ADT name params constructors deconstructor) =
    let paramIds = take (length params) [0..] in
    let paramMap = M.fromList $ zip params paramIds in
    let bindConstructor (Constructor n p) = Constructor n (map (fmap (paramMap M.!)) p) in
    ADT name paramIds (map bindConstructor constructors) deconstructor


type FuncInfo = (Name, Type, Int)

deconstructorInfo :: ADT Id -> FuncInfo
deconstructorInfo adt@(ADT name params constructors _) =
    let retType = TVar (-1) in
    let adttype = TProduct name (map TVar params) in
    let makeF t (Constructor _ p) = foldr (:->) t p in
    let deconstructorType = bind $ foldr ((:->) . makeF retType) (adttype :-> retType) constructors in
    (deconstructorName adt, deconstructorType, length constructors)

constructorsInfo :: ADT Id -> [FuncInfo]
constructorsInfo (ADT name params constructors _) =
    let adttype = TProduct name (map TVar params) in
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
