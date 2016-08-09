
{-|
Module      : Ide3.OrderedMap
Description : Map which preserves the insertion order of keys
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

An OrderedMap functions similar to a standard Map, but when retrieving its keys,
they are returned in the order they are inserted.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.OrderedMap 
    ( OrderedMap
    , empty
    , insert
    , insert'
    , delete
    , lookup
    , keys
    , elems
    , toList
    , fromList
    , filter
    , modifyKey
    ) where

import Prelude hiding (lookup, filter)
import qualified Prelude

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

-- | A wrapper around Int which is used to keep track of the order keys are inserted in
newtype Order = Order Int deriving (Read, Show, Eq, Ord, Enum, Num)

-- | A map which preserves the insertion order of its keys
data OrderedMap k v = OrderedMap
    { 
    -- | Maps keys to value and the order that key was inserted in
      itemMap :: Map k (v, Order)
    -- | Maps the order a key was inserted to the key itself
    , orderMap :: Map Order k
    }
  deriving (Eq)

instance (Show k, Show v, Ord k) => Show (OrderedMap k v) where
    show m = "fromList " ++ (show $ toList m)

instance (Read k, Read v, Ord k) => Read (OrderedMap k v) where
    readsPrec i = \s -> map (firstElem fromList) $ readsPrec i s
      where
        firstElem f (a,b) = (f a,b)

instance Ord k => Foldable (OrderedMap k) where
    foldMap f m = foldMap f $ elems m

-- | An empty map
empty :: Ord k => OrderedMap k v
empty = OrderedMap Map.empty Map.empty

-- | Get the next order of a key to be inserted
nextOrder :: Ord k => OrderedMap k v -> Order
nextOrder m = case Map.maxViewWithKey $ orderMap m of
    Nothing -> 0
    Just ((k,_),_) -> succ k

-- | Insert a value into a map by key
-- If the key exists, it will be moved to the bottom
insert :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v m = m { itemMap = itemMap', orderMap = orderMap' }
  where
    ord = nextOrder m
    itemMap' = Map.insert k (v, ord) $ itemMap m
    orderMap' = case Map.lookup k $ itemMap m of
        Nothing -> Map.insert ord k $ orderMap m
        Just (_,ord') -> Map.insert ord k $ Map.delete ord' $ orderMap m

-- | Insert a value into a map by key
-- If the key exists, it retains its order
insert' :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert' k v m = case Map.lookup k $ itemMap m of
    Nothing -> insert k v m 
    Just (_,ord') -> m{ itemMap = Map.insert k (v,ord') $ itemMap m }

-- | Delete a value
delete :: Ord k => k -> OrderedMap k v -> OrderedMap k v
delete k m = m
    { itemMap = Map.delete k $ itemMap m
    , orderMap = Map.delete ord $ orderMap m
    }
  where
    (_, ord) = itemMap m Map.! k

-- | Check if a map contains a key
lookup :: Ord k => k -> OrderedMap k v -> Maybe v
lookup k = liftM fst . Map.lookup k . itemMap

-- | Retrieve the keys of a map, in the order they were inserted
keys :: Ord k => OrderedMap k v -> [k]
keys = Map.elems . orderMap 

-- | Retreive the elements of a map, in the order their keys were inserted
elems :: Ord k => OrderedMap k v -> [v]
elems m = map (fst . (itemMap m Map.!)) $ keys m

-- | Convert the map to a list of key-value pairs, in order the keys were inserted
toList :: Ord k => OrderedMap k v -> [(k,v)]
toList m = map (\k -> (k, fst $ itemMap m Map.! k)) $ keys m

-- | Create a map from a list of key-value pairs. The order of the keys is taken
-- as the insertion order
fromList :: Ord k => [(k,v)] -> OrderedMap k v
fromList l = OrderedMap 
           { itemMap = Map.fromList itemList
           , orderMap = Map.fromList orderList
           }
  where
    orders = [0..]
    itemList = zipWith makeItemPair l orders
    orderList = zipWith makeOrderPair l orders
    makeItemPair (k,v) i = (k,(v,i))
    makeOrderPair (k,v) i = (i,k)

-- | Create a new map by selecting items from another which pass a predicate,
-- maintaining the relative order of the keys
filter :: Ord k => (v -> Bool) -> OrderedMap k v -> OrderedMap k v
filter f = fromList . Prelude.filter (f . snd) . toList

-- | Replace the key for a value, maintaining the order of the original key
modifyKey :: Ord k => k -> k -> OrderedMap k v -> OrderedMap k v
modifyKey k k' m = m{ itemMap = itemMap', orderMap = orderMap' }
  where
    (v,ord) = itemMap m Map.! k
    itemMap' = Map.insert k' (v,ord) $ Map.delete k $ itemMap m
    orderMap' = Map.insert ord k' $ orderMap m
