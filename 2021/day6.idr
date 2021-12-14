
import Data.Nat
import Data.List
import Data.Vect

import Common

--------------------------------------------------------------------------------

Timer : Type
Timer = Fin 9

Population : Type
Population = Vect 9 Nat

emptyPop : Population
emptyPop = replicate 9 0

--------------------------------------------------------------------------------
-- parsing

fromDigit : Char -> Timer
fromDigit c = case natToFin (cast {to=Nat} (ord c - 48)) 9 of
  Just t  => t
  Nothing => fatal "fromDigit"

parseTimers : String -> List Timer
parseTimers = map fromDigit . filter isDigit . unpack

insertFish : Timer -> Population -> Population
insertFish timer pop = updateAt timer S pop

toPopulation : List Timer -> Population
toPopulation = foldr insertFish emptyPop

parse : String -> Population
parse = toPopulation . parseTimers

--------------------------------------------------------------------------------
-- parts 1 & 2

example : Population
example = toPopulation [3,4,3,1,2]

step : Population -> Population
step (k::ks) = updateAt 6 (+k) ks ++ [k]

solve : Population -> Nat -> Nat
solve pop ndays = sum $ natRec ndays step pop

--------------------------------------------------------------------------------

main : IO ()
main = do
  lines <- readLines "input6"
  let population = parse $ concat lines
  putStrLn "day #6"
  putStrLn $ "part 1 = " ++ show (solve population 80)
  putStrLn $ "part 2 = " ++ show (solve population 256)
