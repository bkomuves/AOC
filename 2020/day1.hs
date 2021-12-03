
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- part1 

solve1 :: [Integer] -> Set Integer
solve1 = Set.fromList . map multiply . findit 

multiply :: (Integer,Integer) -> Integer
multiply (a,b) = a * b

findit :: [Integer] -> [(Integer,Integer)]
findit list = [ (a,b) | a<-list , let b = 2020-a , Set.member b set ] where
  set = Set.fromList list

--------------------------------------------------------------------------------
-- part2

solve2 :: [Integer] -> Set Integer
solve2 = Set.fromList . map multiply3 . findit3

multiply3 :: (Integer,Integer,Integer) -> Integer
multiply3 (a,b,c) = a * b * c

findit3 :: [Integer] -> [(Integer,Integer,Integer)]
findit3 list = [ (a,b,c) | a<-list , b <- list , let c = 2020-a-b , Set.member c set ] where
  set = Set.fromList list

--------------------------------------------------------------------------------

main = do
  text <- readFile "input1"
  print $ solve1 $ map read $ words text
  print $ solve2 $ map read $ words text