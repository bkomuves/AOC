
import qualified Data.Set as Set

import Data.List
import Data.List.Split

parse :: String -> [[String]]
parse = splitWhen null . lines

solve1 :: [String] -> Int
solve1 = length . nub . sort . concat

solve2 :: [String] -> Int
solve2 = Set.size . foldl1' Set.intersection . map Set.fromList

main :: IO ()
main = do
  input <- readFile "input6"
  let xs1 = map solve1 $ parse input
  let xs2 = map solve2 $ parse input
  print $ sum xs1
  print $ sum xs2
