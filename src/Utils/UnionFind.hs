{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Utils.UnionFind (
      UnionFind
    , empty
    , union
    , unionMerge
    , unionMergeFail
    , find
    , equivalent
    , classes
    ) where

import Data.List (intercalate)
import Control.Monad (when, forM)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict (State, evalState, runState, get, put, modify)
import qualified Data.IntMap as IM
import qualified Utils (trace)

trace :: Show a => a -> b -> b
trace = Utils.trace False


type Ptr = Int

data UnionFind a = UnionFind {
    next :: !Ptr,
    imap :: IM.IntMap (Link a),
    hmap :: HashMap a Ptr
}

data Link a
      -- Descriptive element of the equivalence class and its rank.
    = Repr {-# UNPACK #-} !Int a
      -- Pointer to some other element of the equivalence class.
    | Link {-# UNPACK #-} !Ptr
     deriving Show

instance (Show a, Eq a, Hashable a) => Show (UnionFind a) where
    show uf = "fromClasses [" ++ intercalate ", " (map show $ classes uf) ++ "]"


reprInfo :: (Eq a, Hashable a) => a -> State (UnionFind a) (Ptr, Int, a)
reprInfo x = do
    uf@UnionFind{..} <- get
    case HM.lookup x hmap of
        Nothing -> do
            put uf {
                  next = next + 1
                , imap = IM.insert next (Repr 0 x) imap
                , hmap = HM.insert x next hmap
            }
            return (next, 0, x)

        Just n -> go imap n n

    where
        go imap n !i =
          case imap IM.! i of
            Link i' -> do
                (i'', r, a) <- go imap n i'
                -- Path compression
                when (i' /= i'') $
                    modify $ \uf@UnionFind{..} -> uf {
                        imap = IM.insert i (Link i'') imap
                    }
                return (i'', r, a)
            Repr r a -> do
                when (i /= n) $
                    modify $ \uf@UnionFind{..} -> uf {
                        hmap = HM.insert x i hmap
                    }
                return (i, r, a)

findKey :: (Eq a, Hashable a) => a -> State (UnionFind a) Int
findKey x = do
    (i, _, _) <- reprInfo x
    return i



empty :: UnionFind a
empty = UnionFind 0 IM.empty HM.empty

find :: (Eq a, Hashable a) => a -> UnionFind a -> (a, UnionFind a)
find x uf = flip runState uf $ do
    (_, _, a) <- reprInfo x
    return a

equivalent :: (Eq a, Hashable a) => UnionFind a -> a -> a -> Bool
equivalent uf x y = flip evalState uf $ do
    i1 <- findKey x
    i2 <- findKey y
    return $ i1 == i2


union :: (Show a, Eq a, Hashable a) => a -> a -> UnionFind a -> UnionFind a
union = unionMerge const

unionMerge :: (Show a, Eq a, Hashable a) => (a -> a -> a) -> a -> a -> UnionFind a -> UnionFind a
unionMerge merge x y uf = (\(Right x) -> x) $ unionMergeFail ((Right.) . merge) x y uf

unionMergeFail :: (Show a, Eq a, Hashable a) => (a -> a -> Either b a) -> a -> a -> UnionFind a -> Either b (UnionFind a)
unionMergeFail merge x y uf = trace uf $ trace ("union " ++ show x ++ ", " ++ show y) $ flip evalState uf $ do
    (i1, r1, a1) <- reprInfo x
    (i2, r2, a2) <- reprInfo y
    uf@UnionFind{..} <- get
    return $ merge a1 a2 >>= \a ->
        let !imap' = if i1 == i2 then imap else
                case r1 `compare` r2 of
                  LT ->
                    let !imap1 = IM.insert i1 (Link i2) imap in
                    IM.insert i2 (Repr r2 a) imap1
                  EQ ->
                    let !imap1 = IM.insert i1 (Link i2) imap in
                    IM.insert i2 (Repr (r2 + 1) a) imap1
                  GT ->
                    let !imap1 = IM.insert i2 (Link i1) imap in
                    IM.insert i1 (Repr r1 a) imap1
        in Right $ uf {imap = imap'}


classes :: (Eq a, Hashable a) => UnionFind a -> [[a]]
classes = evalState classes'
    where
        classes' :: (Eq a, Hashable a) => State (UnionFind a) [[a]]
        classes' = do
            UnionFind{..} <- get
            elems <- forM (HM.keys hmap) $ \x -> do
                    i <- findKey x
                    return (i, [x])
            return $ IM.elems $ IM.fromListWith (++) elems
