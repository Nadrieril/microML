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


data Pattern a = PVar a | Pattern Name [Pattern a]
    deriving (Functor)

instance PrettyPrint a => PrettyPrint (Pattern a) where
    pprint (PVar v) = pprint v
    pprint (Pattern n l) = "(" ++ unwords (n : map pprint l) ++ ")"


getPatternBinders :: Pattern a -> [a]
getPatternBinders (PVar v) = [v]
getPatternBinders (Pattern _ l) = l >>= getPatternBinders

getPatternADT :: Context -> Pattern a -> (ADT Id, ADT.Constructor Id)
getPatternADT _ (PVar _) = error "Cannot get ADT information for free variable"
getPatternADT ctx (Pattern n _) =
    let Constructor adt i _ = fst $ ctx M.! n in
    (adt, adtConstructors adt !! i)
