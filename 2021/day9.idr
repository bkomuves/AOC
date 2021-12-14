
import Data.Fin
import Data.Vect
import Data.List

import Data.SortedSet

import Common
import Matrix

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
  case parseDigitMatrix ls of
    Nothing   => fatal "not a rectangular shape"
    Just (dim ** hmap) => do
      -- printDigitMatrix {dim=dim} hmap
      part1 hmap
      part2 hmap

