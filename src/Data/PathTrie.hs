module Data.PathTrie where

import Data.Maybe
import Data.Traversable (Traversable(traverse))
import Prelude hiding (lookup)
import qualified Data.Map as M
import Data.String (IsString(..))
import Data.Monoid
import Data.Foldable (Foldable(foldr))

class (Eq a, Ord a, IsString a) => Path a where
  breakup :: a -> [a]
  merge   :: [a] -> a

data PathTrie k a = PathNode (Maybe a) (M.Map k (PathTrie k a))
                    deriving Show

fromList :: Path k => [(k, a)] -> PathTrie k a
fromList ps = foldl ins empty ps
  where ins t (k,v) = insert k v t


toList :: Path k => PathTrie k a -> [(k,a)]
toList = go []
  where go p (PathNode v cs) = el `mappend` concatMap tr items
          where items = M.toList cs
                el = maybe mempty (\x -> [(merge (reverse p), x)]) v
                tr (k,x) = go (k:p) x

empty :: Path a => PathTrie a b
empty = PathNode Nothing M.empty

insert :: Path k => k -> a -> PathTrie k a -> PathTrie k a
insert path val t = ins (breakup path) t
  where
    ins [x] (PathNode v cs) = case M.lookup x cs of
      Nothing -> PathNode v (M.insert x new cs)
      Just (PathNode _ cs') -> PathNode v (M.insert x (PathNode (Just val) cs') cs)
      where new = PathNode (Just val) M.empty

    ins (x:xs) n@(PathNode _ cs) = maybe (alt n empty)
                                         (alt n)
                                         (M.lookup x cs)
      where alt (PathNode v cs) e = PathNode v (M.insert x (ins xs e) cs)

    ins _ _ = empty


lookup :: Path k => k -> PathTrie k a -> Maybe a

lookup p t = lookup' (breakup p) t
  where lookup' (x:xs) (PathNode _ cs) = M.lookup x cs >>= lookup' xs
        lookup' [] (PathNode v _) = v

find :: Path k => k -> PathTrie k a -> Maybe (a, k)
find p t = find' Nothing [] (breakup p) t
  where find' r p (x:xs) (PathNode v cs) = maybe (update p r v)
                                                 (find' (update p r v) (x:p) xs)
                                                 (M.lookup x cs)
        find' r p [] (PathNode v _) = update p r v

        update p r Nothing  = r
        update p r (Just v) = Just (v, merge (reverse p))

instance Functor (PathTrie k) where
  fmap f (PathNode v cs) = PathNode (fmap f v) (fmap (fmap f) cs)

