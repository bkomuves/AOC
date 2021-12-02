
import Data.List
import Data.String
import System.File

import Common

{-
readInt : String -> Int
readInt = cast . trim

pairs : List a -> List (a,a)
pairs Nil            = Nil
pairs (_::Nil)       = Nil
pairs (x::xs@(y::_)) = (x,y) :: pairs xs

triples : List a -> List (a,a,a)
triples Nil               = Nil
triples (_::Nil)          = Nil
triples (_::_::Nil)       = Nil
triples (x::xs@(y::z::_)) = (x,y,z) :: triples xs
-}

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
