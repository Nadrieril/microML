{-# LANGUAGE FlexibleInstances #-}
module Common.Pattern (
      Pattern(..)
    , getPatternBinders
    ) where

import Text.Printf (printf)

import Common.Expr
import Utils.PrettyPrint
import Parse.Token (isOperator)


data Pattern a = PVar a | Pattern Name [Pattern a]
    deriving (Functor, Show)

instance PrettyPrint a => PrettyPrint (Pattern a) where
    pprint (PVar v) = pprint v
    pprint (Pattern n l) = case l of
        [x, y] | isOperator n -> printf "%s %s %s" (pp x) n (pp y)
        l -> printf "%s" $ unwords (n : map pp l)
        where
            pp p@(Pattern _ (_:_))= printf "(%s)" $ pprint p
            pp p = pprint p


getPatternBinders :: Pattern a -> [a]
getPatternBinders (PVar v) = return v
getPatternBinders (Pattern _ l) = l >>= getPatternBinders
