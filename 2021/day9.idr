
import Data.Fin
import Data.Vect
import Data.List

import Data.SortedSet

import Common

--------------------------------------------------------------------------------

public export
Dimensions : Type
Dimensions = Pair Nat Nat

public export
Matrix : Dimensions -> Type -> Type
Matrix (n,m) t = Vect n (Vect m t)

public export
data Index : Dimensions -> Type where
  MkIndex : Fin n -> Fin m -> Index (n,m)

implementation Eq (Index dim) where
  (==) (MkIndex i1 j1) (MkIndex i2 j2) = i1 == i2 && j1 == j2

implementation Ord (Index dim) where
  compare (MkIndex i1 j1) (MkIndex i2 j2) = case compare i1 i2 of
    LT => LT
    GT => GT
    EQ => compare j1 j2

implementation Show (Index dim) where
  show (MkIndex i j) = show (i,j)

-- NB: you have to pattern match on `dim` because lack of eta equality in Idris...
public export
index : {0 dim : Dimensions} -> Index dim -> Matrix dim a -> a
index {dim=(_,_)} (MkIndex i j) mat = Vect.index j $ Vect.index i mat

allIndices : (dim : Dimensions) -> List (Index dim)
allIndices (n,m) = [ MkIndex i j | i <- toList range, j <- toList range ]

namespace Dim1
  public export
  neighbours : {n : Nat} -> Fin n -> List (Fin n)
  neighbours {n = S Z        } FZ      = []
  neighbours {n = S (S Z)    } (FS FZ) = [FZ]
  neighbours {n = S (S m)    } (FZ   ) = [FS FZ]
  neighbours {n = S (S (S m))} (FS FZ) = [FZ, FS (FS FZ)]
  neighbours {n = S (S m)    } (FS k ) = map FS (neighbours k)

namespace Dim2
  public export
  neighbours : {dim : Dimensions} -> Index dim -> List (Index dim)
  neighbours {dim=(_,_)} (MkIndex i j) = [ (MkIndex a j) | a <- Dim1.neighbours i ]
                                      ++ [ (MkIndex i b) | b <- Dim1.neighbours j ]
--------------------------------------------------------------------------------
-- parsing

parseLine : (m : Nat) -> String -> Maybe (Vect m Digit)
parseLine m str = case allJust $ map parseDigit $ unpack str of
  Nothing => Nothing
  Just xs => fromListN m xs

parseHeightMap' : (dim : Dimensions) -> List String -> Maybe (Matrix dim Digit)
parseHeightMap' (n,m) ls = fromListN n =<< allJust (map (parseLine m) ls)

parseHeightMap : List String -> Maybe (dim : Dimensions ** Matrix dim Digit)
parseHeightMap ls =
  let n = length ls
      m = case ls of { (l::_) => length l ; _ => 0 }
      dim = (n,m)
  in  case parseHeightMap' dim ls of
        Just mat => Just (dim ** mat)
        Nothing  => Nothing

printLine : {m : Nat} -> Vect m Digit -> IO ()
printLine vect = do
  putStrLn $ concat [ showDigit (index j vect) | j <- toList (range {len=m}) ]

printMatrix : {dim : Dimensions} -> Matrix dim Digit -> IO ()
printMatrix {dim=(_,_)} mat = mapM_ printLine mat

--------------------------------------------------------------------------------
-- part 1

findLowPoints : {dim : Dimensions} -> Matrix dim Digit -> List (Index dim, Digit)
findLowPoints {dim} matrix = mapMaybe mbLow (allIndices dim) where

  mbLow : Index dim -> Maybe (Index dim, Digit)
  mbLow ij = let this   = index ij matrix
                 neighs = neighbours ij
                 those  = map (\ij => index ij matrix) neighs
             in  if all (>this) those
                   then Just (ij, this)
                   else Nothing

part1 : {dim : Dimensions} -> Matrix dim Digit -> IO ()
part1 matrix = do
  putStrLn "\npart 1:"
  let low = map snd $ findLowPoints matrix
  -- putStrLn $ "low = " ++ show low
  putStrLn $ "answer = " ++ show (sum [ S (digitToNat d) | d <- low ])

--------------------------------------------------------------------------------
-- part 2

aboveNeighbours : {dim : Dimensions} -> Matrix dim Digit -> Index dim -> List (Index dim)
aboveNeighbours mat pos = filter cond (neighbours pos) where
  height : Digit
  height = index pos mat
  cond : Index dim -> Bool
  cond ij = let h = index ij mat in h /= 9 && h > height

Basin : Dimensions -> Type
Basin dim = SortedSet (Index dim)

basinSize : Basin dim -> Nat
basinSize = length . SortedSet.toList

findBasin : {dim : Dimensions} -> Matrix dim Digit -> Index dim -> Basin dim
findBasin matrix lowest = worker empty [lowest] where

  worker : Basin dim -> List (Index dim) -> Basin dim
  worker basin Nil = basin
  worker basin (loc::locs) = if contains loc basin
    then worker basin locs
    else let locs' = locs ++ filter (\x => not (contains x basin)) (aboveNeighbours matrix loc)
         in  worker (insert loc basin) locs'

part2 : {dim : Dimensions} -> Matrix dim Digit -> IO ()
part2 matrix = do
  putStrLn "\npart 2:"
  let low = map fst $ findLowPoints matrix
  let basins = map (findBasin matrix) low
  -- mapM_ (printLn . SortedSet.toList) basins
  let sizes = reverse $ sort $ map basinSize basins
  -- printLn sizes
  case sizes of
    (a::b::c::_) => do
      putStrLn $ "top 3  = " ++ show (the (List Nat) [a,b,c])
      putStrLn $ "answer = " ++ show (a*b*c)
    _ => fatal "there are less than 3 basins"

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input9"
  case parseHeightMap ls of
    Nothing   => fatal "not a rectangular shape"
    Just (dim ** hmap) => do
      -- printMatrix {dim=dim} hmap
      part1 hmap
      part2 hmap

