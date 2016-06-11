{-# LANGUAGE FlexibleInstances #-}
module Common.Pattern (
      Pattern(..)
    , getPatternBinders
    , getPatternADT
    ) where

import qualified Data.Map as M
import Text.Printf (printf)

import qualified Common.ADT as ADT
import Common.ADT hiding (Constructor)
import Common.Context
import Common.Expr
import Utils.PrettyPrint
import Parse.Token (isOperator)


data Pattern a = PVar a | Pattern Name [Pattern a]
    deriving (Functor)

instance PrettyPrint a => PrettyPrint (Pattern a) where
    pprint (PVar v) = pprint v
    pprint (Pattern n l) = case l of
        [x, y] | isOperator n -> printf "%s %s %s" (pp x) n (pp y)
        l -> printf "%s" $ unwords (n : map pp l)
        where
            pp p@(Pattern _ (_:_))= printf "(%s)" $ pprint p
            pp p = pprint p


getPatternBinders :: Pattern a -> [a]
getPatternBinders (PVar v) = [v]
getPatternBinders (Pattern _ l) = l >>= getPatternBinders

getPatternADT :: Context -> Pattern a -> (ADT Id, ADT.Constructor Id)
getPatternADT _ (PVar _) = error "Cannot get ADT information for free variable"
getPatternADT ctx (Pattern n _) =
    let Constructor adt i _ = fst $ ctx M.! n in
    (adt, adtConstructors adt !! i)
