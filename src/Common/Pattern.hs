{-# LANGUAGE FlexibleInstances #-}
module Common.Pattern (
      Pattern(..)
    , getPatternBinders
    , getPatternADT
    ) where

import qualified Data.Map as M

import qualified Common.ADT as ADT
import Common.ADT hiding (Constructor)
import Common.Context
import Common.Expr


data Pattern a = Pattern Name [a]
    deriving (Functor)

instance PrettyPrint a => PrettyPrint (Pattern a) where
    pprint (Pattern n l) = unwords (n : map pprint l)


getPatternBinders :: Pattern a -> [a]
getPatternBinders (Pattern _ l) = l

getPatternADT :: Context -> Pattern a -> (ADT Id, ADT.Constructor Id)
getPatternADT ctx (Pattern n _) =
    let Constructor adt i _ = fst $ ctx M.! n in
    (adt, adtConstructors adt !! i)
