module Utils.PrettyPrint where

class PrettyPrint a where
    pprint :: (?toplevel :: Bool) => a -> String

instance PrettyPrint String where
    pprint = id

instance PrettyPrint Int where
    pprint = show
