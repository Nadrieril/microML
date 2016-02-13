{-# LANGUAGE OverloadedStrings #-}
module Common.ADT where


import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad (join)
import Control.Arrow((&&&), second)
-- import qualified Debug.Trace as T

import Common.Expr
import Common.Type

data ADT a = ADT {
      adtName :: Name
    , adtParams :: [Name]
    , adtConstructors :: [Constructor a]
    , deconstructor :: Maybe Name
    } deriving (Show)

data Constructor a = Constructor {
      constructorName :: Name
    , constructorParams :: [Mono a]
    } deriving (Show)


deconstructorName :: ADT a -> Name
deconstructorName ADT{..} = fromMaybe (Name ("un" ++ show adtName)) deconstructor

adtTypes :: ADT Name -> [(Name, Type)]
adtTypes adt@(ADT adtName params constructors _) =
    -- Use negative variable ids to avoid collision on intanciation
    let paramIds = take (length params) $ map negate [2..] in
    let paramMap = M.fromList $ zip params paramIds in
    let boundConstructors = map (\(Constructor n p) -> Constructor n (map (fmap (paramMap M.!)) p)) constructors in
    let makeF t (Constructor _ p) = foldr (:->) t p in

    let adttype = TProduct adtName (map TVar paramIds) in
    let deconstructor = foldr ((:->) . makeF (TVar (-1))) (adttype :-> TVar (-1)) boundConstructors in
    let name (Constructor n _) = n in
    map (second bind) $
        (deconstructorName adt, deconstructor) : map (name &&& makeF adttype) boundConstructors


adts :: [ADT Name]
adts = [pair, option, list]

adtMap :: M.Map Name (ADT Name, Type)
adtMap = M.fromList $ join $ fmap (\adt -> second (\t -> (adt, t)) <$> adtTypes adt) adts

adtFunMap :: M.Map Name Type
adtFunMap = fmap snd adtMap

constructorsMap :: ADT a -> M.Map Name (Constructor a)
constructorsMap ADT{..} = M.fromList $ fmap (constructorName &&& id) adtConstructors


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
