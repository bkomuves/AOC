
import Data.List
import Data.String
import System.File

import Common 

tripleSum : (Int,Int,Int) -> Int
tripleSum (x,y,z) = x + y + z

pairDiff : (Int,Int) -> Int
pairDiff (x,y) = y - x

solve1 : List Int -> Nat
solve1 = length . filter (\d => d > 0) . map pairDiff . pairs

solve2 : List Int -> Nat
solve2 = length . filter (\d => d > 0) . map pairDiff . pairs . map tripleSum . triples

main : IO ()
main = do
  lines <- readLines "input1"
  let input = map readInt lines
  printLn $ solve1 input
  printLn $ solve2 input
