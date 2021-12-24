
import Data.Fin
import Data.Vect
import Data.List

import Data.SortedSet
import Data.SortedMap

import Control.Monad.State

import Common
import Matrix
import Digit

--------------------------------------------------------------------------------

RiskMap : Dimensions -> Type
RiskMap dim = Matrix dim Nat

--------------------------------------------------------------------------------
-- part 1

Table : Dimensions -> Type
Table dim = SortedMap (Index dim) Nat

-- we "simulate" a priority queue via this
Unvisited : Dimensions -> Type
Unvisited dim = SortedMap Nat (SortedSet (Index dim))

namespace Unvisit

  export
  member : Index dim -> Nat -> Unvisited dim -> Bool
  member idx dist unvisited = case lookup dist unvisited of
    Nothing  => False
    Just set => contains idx set

  export
  insert : Index dim -> Nat -> Unvisited dim -> Unvisited dim
  insert idx dist unvisited = alter dist f unvisited where
    f : Maybe (SortedSet (Index dim)) -> Maybe (SortedSet (Index dim))
    f Nothing    = Just (singleton idx )
    f (Just set) = Just (insert idx set)

  export
  remove : Index dim -> Nat -> Unvisited dim -> Unvisited dim
  remove idx dist unvisited = alter dist f unvisited where
    f : Maybe (SortedSet (Index dim)) -> Maybe (SortedSet (Index dim))
    f Nothing    = Nothing
    f (Just set) = let set1 = delete idx set in if null set1
      then Nothing
      else Just set1

record St (n, m : Nat) where
  constructor MkSt
  table     : Table (n,m)
  unvisited : Unvisited (n,m)

-- this is a rather shitty (and painful!) implementation of Dijkstra's shorthest path algorithm
dijkstra : {dim : Dimensions} -> {auto prf : PosDim dim} -> Matrix dim Nat -> IO Nat
dijkstra {dim=(n,m)} matrix = evalStateT iniSt action where

  infinity : Nat
  infinity = n*m*11

  start, end : Index (n,m)
  start = topLeftCorner     -- (n,m)
  end   = bottomRightCorner -- (n,m)

  lkp : Index (n,m) -> Table (n,m) -> Nat
  lkp idx table = fromMaybe infinity (SortedMap.lookup idx table)

  iniSt : St n m
  iniSt = MkSt
    (singleton start 0)
    (singleton infinity (delete start (SortedSet.fromList (allIndices (n,m)))))

  isUnvisited : St n m -> Index (n,m) -> Bool
  isUnvisited (MkSt table unvisited) idx = member idx (lkp idx table) unvisited

  markVisited : Index (n,m) -> StateT (St n m) IO ()

  worker : Index (n,m) -> StateT (St n m) IO ()
  worker idx = if idx == end then pure () else do

    st@(MkSt table unvisited) <- get
    let current = lkp idx table
    let neighs  = filter (isUnvisited st) (neighbours idx)

    forM_ neighs $ \jdx => do
      MkSt table unvisited <- get
      let prev = lkp   jdx table
      let risk = index jdx matrix
      let via  = current + risk
      when (via < prev) $ do
        let unvisited1
              = insert jdx via
              $ remove jdx prev
              $ unvisited
        let table1 = insert jdx via table
        put $ MkSt table1 unvisited1

    MkSt table unvisited <- get
    let unvisited1 = remove idx current unvisited
    put $ MkSt table unvisited1

    case leftMost unvisited1 of
      Nothing         => pure ()
      Just (dist,set) => case SortedSet.toList set of
        Nil       => fatal "dijsktra/worker: should not happen"
        (next::_) => worker next

  action : StateT (St n m) IO Nat
  action = do
    worker start
    MkSt table _ <- get
    pure (lkp end table)

part1 : {dim : Dimensions} -> {auto prf : PosDim dim} -> RiskMap dim -> IO ()
part1 riskmap = do
  putStrLn "\npart 1"
  answer <- (dijkstra riskmap)
  putStrLn $ "answer = " ++ show answer

--------------------------------------------------------------------------------
-- part 2

wraparound1 : Digit -> Digit
wraparound1 x = case incFin x of
  Nothing => 1
  Just y  => y

wraparoundN : Nat -> Digit -> Digit
wraparoundN k = natRec k wraparound1

wraparoundTile : {0 dim : Dimensions} -> Nat -> Matrix dim Digit -> Matrix dim Digit
wraparoundTile k = map @{matrix} (wraparoundN k)

wraparoundTile2 : {0 n, m : Nat} -> Nat -> Matrix (n,m) Digit -> Matrix (n,m) Digit
wraparoundTile2 = wraparoundTile {dim=(n,m)}

joinTiles : {k,l,n,m : Nat} -> Matrix (k,l) (Matrix (n,m) a) -> Matrix (k*n, l*m) a
joinTiles tiles = mkMatrix f where
  f : Index (k*n, l*m) -> a
  f (MkIndex a b) =
    let (s,i) = finDivMod a
        (t,j) = finDivMod b
    in  index (MkIndex i j) $ index (MkIndex s t) tiles

part2 : {dim : Dimensions} -> {auto prf : PosDim dim} -> Matrix dim Digit -> IO ()
part2 small = case dim of (n,m) => do
  putStrLn "\npart 2"
  let tiles : Matrix (5,5) (Matrix (n,m) Digit)
      tiles = mkMatrix {dim=(5,5)} $ \(MkIndex i j) => wraparoundTile2 (finToNat i + finToNat j) small
  let big = joinTiles tiles
  -- printDigitMatrix {dim=(5*n,5*m)} big
  let riskmap = mapMatrix {dim=(5*n,5*m)} digitToNat big
  let prf2 = scalePosDim 5 prf
  answer <- (dijkstra {dim=(5*n,5*m)} {prf=prf2} riskmap)
  putStrLn $ "answer = " ++ show answer

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input15"
  case parseDigitMatrix ls of
    Nothing   => fatal "not a rectangular shape"
    Just (dim ** riskmap_) => case isPosDim dim of
      Nothing  => fatal "matrix has zero dimension"
      Just prf => do
        -- printDigitMatrix {dim=dim} riskmap_
        let riskmap = map @{matrix} digitToNat riskmap_
        part1 riskmap
        part2 riskmap_
