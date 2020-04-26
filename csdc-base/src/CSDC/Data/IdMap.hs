module CSDC.Data.IdMap
  ( IdMap (..)
  , empty
  , lookup
  , find
  , insert
  , insertNew
  , delete
  , filter
  ) where

import CSDC.Data.Id (Id (..), zero, next)

import Data.Aeson (ToJSON, FromJSON)
import Data.IntMap.Strict (IntMap)

import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap

import Prelude hiding (lookup, filter)

--------------------------------------------------------------------------------
-- Type definition

newtype IdMap a = IdMap { getIdMap :: IntMap a }
  deriving newtype (Show, Eq, Functor, Foldable, ToJSON, FromJSON)
  deriving stock (Traversable)

empty :: IdMap a
empty = IdMap (IntMap.empty)

lookup :: Id a -> IdMap a -> Maybe a
lookup (Id uid) (IdMap m) = IntMap.lookup uid m

find :: (a -> Bool) -> IdMap a -> Maybe a
find p (IdMap m) = List.find p $ IntMap.elems m

insert :: Id a -> a -> IdMap a -> IdMap a
insert (Id uid) a (IdMap m) = IdMap $ IntMap.insert uid a m

insertNew :: a -> IdMap a -> (Id a, IdMap a)
insertNew a idmap@(IdMap m) =
  let
    uid =
      case IntMap.lookupMax m of
        Nothing -> zero
        Just (uid',_) -> next (Id uid')
  in
    (uid, insert uid a idmap)

delete :: Id a -> IdMap a -> IdMap a
delete (Id uid) (IdMap m) = IdMap $ IntMap.delete uid m

filter :: (a -> Bool) -> IdMap a -> IdMap a
filter f (IdMap m) = IdMap $ IntMap.filter f m
