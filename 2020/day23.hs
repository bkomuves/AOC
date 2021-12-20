
{-# LANGUAGE Strict, NumericUnderscores #-}

import Control.Monad
import Data.Char

import Data.Array.IArray
import Data.Array.MArray
import Data.Array.IO
import Data.Array.Unboxed

--------------------------------------------------------------------------------

example = "389125467"
input   = "562893147"

--------------------------------------------------------------------------------
-- part 1

parseInput :: String -> (Int, UArray Int Int)
parseInput = encode . parseInputToList

parseInputToList :: String -> [Int]
parseInputToList str = map (\c -> ord c - 48) str

cyclicPairs :: [a] -> [(a,a)]
cyclicPairs []      = error "cyclicPairs: empty" 
cyclicPairs (x0:xs) = go x0 xs where
  go x []     = [(x,x0)]
  go x (y:ys) = (x,y) : go y ys

-- the encoding is: the index is a _label_ and the value the _label_ of 
-- the clockwise next one.
encode :: [Int] -> (Int, UArray Int Int)
encode list = (head list, arr) where
  n     = length list
  input = listArray (1,n) list :: UArray Int Int
  arr   = array (1,n) (cyclicPairs list)

decode :: (Int, UArray Int Int) -> [Int]
decode (start,arr) = go start where
  go pos = let next = arr!pos in if next/=start then pos : go next else [pos]

decWhileNotElem :: Int -> Int -> [Int] -> Int
decWhileNotElem n k forbidden = 
  let k1 = if k == 1 then n else (k - 1)
  in  if elem k1 forbidden then decWhileNotElem n k1 forbidden else k1

step :: IOUArray Int Int -> Int -> IO Int
step marr current = do
  (1,n) <- getBounds marr
  next1 <- readArray marr current
  next2 <- readArray marr next1
  next3 <- readArray marr next2  
  jump  <- readArray marr next3
  let dest = decWhileNotElem n current [next1,next2,next3]
  -- print (current,[next1,next2,next3],dest,jump)
  after <- readArray marr dest
  writeArray marr current jump
  writeArray marr dest    next1 
  writeArray marr next3   after
  return jump

nsteps :: Int -> (Int, UArray Int Int) -> IO (Int, UArray Int Int) 
nsteps n (cur,iarr) = do
  marr  <- thaw iarr
  final <- foldM (\k _ -> step marr k) cur [1..n] 
  farr  <- freeze marr
  return $ (final,farr)

solve1 :: String -> Int -> IO ()
solve1 input n = do
  (_,arr) <- nsteps n $ parseInput input
  let list = decode (1, arr)
  -- print list
  putStrLn $ "the answer is = " ++ concatMap show (tail list)

--------------------------------------------------------------------------------
-- part 2  

parseInputV2 :: String -> (Int, UArray Int Int)
parseInputV2 = encode . parseInputToListV2

parseInputToListV2 :: String -> [Int]
parseInputToListV2 str = map (\c -> ord c - 48) str ++ [(length str + 1)..1_000_000]

solve2 :: String -> Int -> IO ()
solve2 input n = do
  (_,arr) <- nsteps n $ parseInputV2 input
  let next1 = arr ! 1
  let next2 = arr ! next1
  print [next1,next2]
  putStrLn $ "the answer is = " ++ show (next1 * next2)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "\npart 1"
  solve1 input 100
  putStrLn "\npart 2"
  solve2 input 10_000_000
