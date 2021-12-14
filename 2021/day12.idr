
import Data.List
import Data.List1
import Data.Vect
import Data.String

import Data.SortedSet
import Data.SortedMap

import Control.Monad.Identity
import Control.Monad.Writer

import Common

--------------------------------------------------------------------------------

data Size
  = Small
  | Big

sizeOf : String -> Size
sizeOf str = case strUncons str of
  Just (c,_) => if c <= 'Z' then Big else Small
  Nothing    => fatal "sizeOf: empty cave name"

record Edge (v : Type) where
  constructor MkEdge
  fromVtx : v
  toVtx   : v

implementation Show v => Show (Edge v) where
  show (MkEdge src tgt) = show src ++ " -> " ++ show tgt

Functor Edge where
  map f (MkEdge x y) = MkEdge (f x) (f y)

flipEdge : Edge v -> Edge v
flipEdge (MkEdge x y) = MkEdge y x

parseEdge : String -> Edge String
parseEdge str = case split (=='-') str of
  (from:::[to]) => MkEdge from to
  _             => fatal $ "cannot parse edge"

--------------------------------------------------------------------------------
-- graphs

record Graph (v : Type) where
  constructor MkGraph
  fwdMap : SortedMap v (SortedSet v)
  bwdMap : SortedMap v (SortedSet v)

empty : Ord v => Graph v
empty = MkGraph empty empty

member : Ord v => Edge v -> Graph v -> Bool
member (MkEdge x y) (MkGraph fwd _) = case lookup x fwd of
  Nothing  => False
  Just set => contains y set

insertDirected : Ord v => Edge v -> Graph v -> Graph v
insertDirected (MkEdge x y) (MkGraph fwd bwd) = MkGraph (insert1 x y fwd) (insert1 y x bwd) where

  insert1 : v -> v -> SortedMap v (SortedSet v) -> SortedMap v (SortedSet v)
  insert1 src tgt table = case lookup src table of
    Nothing  => insert src (singleton tgt    ) table
    Just set => insert src (insert    tgt set) table

insertUndirected : Ord v => Edge v -> Graph v -> Graph v
insertUndirected edge g
  = insertDirected (flipEdge edge)
  $ insertDirected edge g

fromEdgeList : Ord v => List (Edge v) -> Graph v
fromEdgeList = foldr insertUndirected empty

toEdgeList : Graph v -> List (Edge v)
toEdgeList (MkGraph fwd _) = concat
  [ [ MkEdge src tgt | tgt <- SortedSet.toList tgtset ] | (src,tgtset) <- SortedMap.toList fwd ]

-- direction along an oriented edge
data Dir
  = Fwd
  | Bwd

neighbourSet : Ord v => Dir -> Graph v -> v -> SortedSet v
neighbourSet Fwd graph x = fromMaybe empty $ SortedMap.lookup x (graph.fwdMap)
neighbourSet Bwd graph x = fromMaybe empty $ SortedMap.lookup x (graph.bwdMap)

neighbourList : Ord v => Dir -> Graph v -> v -> List v
neighbourList dir graph x = SortedSet.toList (neighbourSet dir graph x)

--------------------------------------------------------------------------------
-- part 1

Path : Type -> Type
Path vtx = List vtx

V : Type
V = String

E : Type
E = Edge V

G : Type
G = Graph V

P : Type
P = Path V

-- this is in IO for easier debugging
findPaths : G -> IO (List P)
findPaths graph = execWriterT (go empty [] "start") where

  go : SortedSet V -> P -> V -> WriterT (List P) IO ()
  go visited path vtx = if contains vtx visited
    then pure ()
    else do
      let path' = vtx :: path
      let next = neighbourList Fwd graph vtx
      -- putStrLn $ "at " ++ show (reverse path) ++ " : " ++ show vtx ++ " -> " ++ show next
      let visited' = case sizeOf vtx of
            Small => insert vtx visited
            Big   => visited
      if vtx == "end"
        then do
          let path = reverse path'
          tell [path]
          -- putStrLn $ "finished: " ++ show path
        else mapM_ (go visited' path') next

part1 : G -> IO ()
part1 graph = do
  putStrLn "part 1"
  -- mapM_ printLn $ toEdgeList graph
  ps <- findPaths graph
  -- mapM_ printLn ps
  putStrLn $ "number of paths = " ++ show (length ps)

--------------------------------------------------------------------------------

-- this is in IO for easier debugging
findPathsV2 : G -> IO (List P)
findPathsV2 graph = execWriterT (go Nothing empty [] "start") where

  go : Maybe V -> SortedSet V -> P -> V -> WriterT (List P) IO ()
  go mbDbl visited path vtx = if (contains vtx visited && (isJust mbDbl || vtx == "start"))
    then pure ()
    else do
      let path' = vtx :: path
      let next = neighbourList Fwd graph vtx
      let mbDbl' = case mbDbl of
            Just _  => mbDbl
            Nothing => if contains vtx visited then Just vtx else Nothing
      let visited' = case sizeOf vtx of
            Small => insert vtx visited
            Big   => visited
      if vtx == "end"
        then do
          let path = reverse path'
          tell [path]
          -- putStrLn $ "finished: " ++ show path
        else mapM_ (go mbDbl' visited' path') next

part2 : G -> IO ()
part2 graph = do
  putStrLn "part 2"
  ps <- findPathsV2 graph
  putStrLn $ "number of paths = " ++ show (length ps)

--------------------------------------------------------------------------------
main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input12"
  let edges = map parseEdge ls
  let graph = fromEdgeList edges
  part1 graph
  part2 graph
