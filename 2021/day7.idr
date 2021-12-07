
import Data.List
import Data.List1
import Data.String

import Common

--------------------------------------------------------------------------------

Pos : Type
Pos = Int

Input : Type
Input = List1 Pos

parse : String -> Input
parse str = map readInt $ String.split (==',') str

--------------------------------------------------------------------------------

unsafeToList1 : List a -> List1 a
unsafeToList1 xs = case fromList xs of
  Just l1 => l1
  Nothing => fatal "unsafeToList1"

unsafeHead : List a -> a
unsafeHead (x::_) = x
unsafeHead _      = fatal "unsafeHead"

--------------------------------------------------------------------------------
-- part 1

ex1 : Input
ex1 = toList1 [16,1,2,0,4,2,7,1,2,14]

Fuel : Type
Fuel = Int

fuel : Pos -> Input -> Fuel
fuel pos input = sum $ map (\x => abs (x-pos)) input

minimum, maximum : Ord a => List1 a -> a
minimum = foldl1 min
maximum = foldl1 max

bounds : Ord a => List1 a -> (a,a)
bounds xs = (minimum xs, maximum xs)

solve1 : Input -> Pos
solve1 input = fst $ unsafeHead $ sortBy (comparing snd) candidates where
  ab : (Pos,Pos)
  ab = bounds input
  candidates : List (Pos,Fuel)
  candidates = [ (p, fuel p input) | p<-[fst ab..snd ab] ]

part1 : Input -> IO ()
part1 input = do
  putStrLn "\npart 1"
  let sol = solve1 input
  putStrLn $ "target    = " ++ show sol
  putStrLn $ "fuel req. = " ++ show (fuel sol input)

--------------------------------------------------------------------------------

round : Double -> Int
round x = cast (x+0.5)

halfSquare : Int -> Int
halfSquare y = f (abs y) where
  f : Int -> Int
  f x = div (x*(x+1)) 2

fuel2 : Pos -> Input -> Fuel
fuel2 pos input = sum $ map (\x => halfSquare (x-pos)) input

solve2 : Input -> Pos
solve2 input = fst $ unsafeHead $ sortBy (comparing snd) candidates where
  ab : (Pos,Pos)
  ab = bounds input
  candidates : List (Pos,Fuel)
  candidates = [ (p, fuel2 p input) | p<-[fst ab..snd ab] ]

part2 : Input -> IO ()
part2 input = do
  putStrLn "\npart 2"
  let sol = solve2 input
  putStrLn $ "average   = " ++ show avg          -- least squares solution
  putStrLn $ "rounded   = " ++ show (round avg)
  putStrLn $ "target    = " ++ show sol
  putStrLn $ "lsq. fuel req. = " ++ show (fuel2 (round avg) input)
  putStrLn $ "tgt. fuel req. = " ++ show (fuel2  sol        input)

  where

    n : Nat
    n = length $ forget input

    flts : List1 Double
    flts = map cast input

    avg : Double
    avg = sum (forget flts) / cast n

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input7"
  let input = parse $ trim $ concat ls
  part1 input
  part2 input
