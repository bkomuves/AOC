
-- Dijkstra's shortest path algorithm
-- based on <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>

module Dijkstra

import Data.Maybe
import Data.List
import Data.SortedSet
import Data.SortedMap

import Control.Monad.State

import Common

--------------------------------------------------------------------------------

-- this is a rather bad priority queue, but the Idris ecosystem is not mature yet,
-- and I couldn't bother trying to do anything better right now...
namespace Prio

  export
  record PrioQueue (dist : Type) (a : Type) where
    constructor MkQ
    keyLkp  : SortedMap a dist
    distLkp : SortedMap dist (SortedSet a)

  empty : (Ord dist, Ord a) => PrioQueue dist a
  empty = MkQ empty empty

  export
  member : (Ord a) => a -> PrioQueue dist a -> Bool
  member key (MkQ ktable _) = isJust (lookup key ktable)

  export
  lookup : (Ord a) => a -> PrioQueue dist a -> Maybe dist
  lookup key (MkQ ktable _) = lookup key ktable

  export 
  findSmallest : PrioQueue dist a -> Maybe (a,dist)
  findSmallest (MkQ ktable dtable) = case leftMost dtable of 
    Nothing      => Nothing
    Just (d,set) => case SortedSet.toList set of
      Nil          => fatal "PrioQueue/findSmallest: this should not happen"
      (next::_)    => Just (next,d)

  export
  remove : (Ord dist, Ord a) => a -> PrioQueue dist a -> PrioQueue dist a
  remove key old@(MkQ ktable dtable) = case lookup key ktable of
    Nothing   => old
    Just dist =>
      let ktable' = SortedMap.delete key ktable
          dtable' = alter dist f dtable
      in MkQ ktable' dtable'
    where
      f : Maybe (SortedSet a) -> Maybe (SortedSet a)
      f Nothing    = Nothing
      f (Just set) = let set1 = delete key set in if null set1
        then Nothing
        else Just set1

  unsafeInsert : (Ord dist, Ord a) => a -> dist -> PrioQueue dist a -> PrioQueue dist a
  unsafeInsert key dist (MkQ ktable dtable) = 
    let ktable' = SortedMap.insert key dist ktable
        dtable' = alter dist f dtable
    in MkQ ktable' dtable'
    where
      f : Maybe (SortedSet a) -> Maybe (SortedSet a)
      f Nothing    = Just (singleton key )
      f (Just set) = Just (insert key set)

  export
  insert : (Ord dist, Ord a) => a -> dist -> PrioQueue dist a -> PrioQueue dist a
  insert key dist queue = if member key queue
    then unsafeInsert key dist $ remove key queue
    else unsafeInsert key dist queue

  export
  insertIfSmaller : (Ord dist, Ord a) => a -> dist -> PrioQueue dist a -> PrioQueue dist a
  insertIfSmaller key dist queue = case lookup key queue of
    Nothing => unsafeInsert key dist queue
    Just d  => if dist < d 
      then unsafeInsert key dist $ remove key queue
      else queue

--------------------------------------------------------------------------------

public export
data AddInf a 
  = Finite a
  | Infinity

public export
Eq a => Eq (AddInf a) where
  (==) (Finite x) (Finite y) = x == y
  (==) Infinity   Infinity   = True
  (==) _          _          = False

public export
Ord a => Ord (AddInf a) where
  compare (Finite x) (Finite y) = compare x y
  compare (Finite _) Infinity   = LT
  compare Infinity   (Finite _) = GT
  compare Infinity   Infinity   = EQ

public export
Show a => Show (AddInf a) where
  show (Finite x) = show x
  show Infinity   = "infinity"

--------------------------------------------------------------------------------

Unvisited : Type -> Type -> Type
Unvisited node dist = PrioQueue dist node -- (AddInf dist) node

record S (node : Type) (dist : Type) where
  constructor MkS
  visited   : SortedSet node                   -- we don't know the list of all nodes at the beginning
  tentative : SortedMap node dist              -- this is only needed to reconstruct a shortest path?
  unvisited : Unvisited node dist

--------------------------------------------------------------------------------

export
dijkstra_ : (Ord node, Ord dist, Num dist) => (node -> List (node,dist)) -> (node,dist) -> node -> AddInf dist
dijkstra_ neighbours (start,zero) destination = evalState iniState (worker start zero) where

  iniState : S node dist
  iniState = MkS empty (singleton start zero) empty

  unsafeLookupUnv : node -> Unvisited node dist -> dist
  unsafeLookupUnv node queue = case Prio.lookup node queue of
    Nothing => fatal "dijkstra/unsafeLookupUnv: not found"
    Just d  => d

  worker : node -> dist -> State (S node dist) (AddInf dist)
  worker this c = if this == destination 

    then pure (Finite c)

    else do
      MkS vis tent unv <- get
      let neighs    = neighbours this
          unvNeighs = filter (\(n,d) => not (contains n vis)) neighs
      let vis1  = insert this vis
          unv1  = foldl (\q, (n,d) => insertIfSmaller n (c+d) q          ) unv  $ unvNeighs
          tent1 = foldl (\t,  n    => insert n (unsafeLookupUnv n unv1) t) tent $ map fst unvNeighs
      put $ MkS vis1 tent1 (remove this unv1)

      MkS _ _ unv <- get 
      case findSmallest unv of
        Nothing    => pure Infinity
        Just (n,d) => worker n d

--------------------------------------------------------------------------------

-- dijkstra : (Ord node, Ord dist) => (node -> List (dist,node)) -> node -> node -> (List node, dist)
-- dijkstra neighs start dest = fst (dijsktra neighs start dest)

--------------------------------------------------------------------------------

