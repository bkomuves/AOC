
{-# LANGUAGE Strict #-}

import Data.Array
import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

check' :: Int -> [Int] -> Maybe Int
check' n list = go (take n list) (drop n list) where
  go _ [] = Nothing
  go preamble (this:rest) = 
    if elem this sumtwo
      then go preamble' rest
      else Just this
    where
      sumtwo    = [ a + b | a <- preamble , b <- preamble , a /= b ]
      preamble' = tail preamble ++ [this]

findCont :: [Int] -> Int -> [Int]
findCont list tgt = result where
  result = head [ [ arr!k | k<-[a+1..b] ] | a<-[0..n-2] , b<-[2..n] , cumsum!b - cumsum!a == tgt ] 
  arr    = listArray (1,n) $ list
  cumsum = listArray (0,n) $ scanl (+) 0 list
  n = length list

--------------------------------------------------------------------------------

test1 = do
  xs <- (map read . words) <$> readFile "test9"
  let solution = check' 5 xs
  print solution
  return $ fromJust solution

solve1 = do
  xs <- (map read . words) <$> readFile "input9"
  let solution = check' 25 xs
  print solution
  return $ fromJust solution

--------------------------------------------------------------------------------

test2 invalid = do
  xs <- (map read . words) <$> readFile "test9"
  let stuff = (findCont xs invalid)
  print (stuff)
  print (sum stuff)
  print (minimum stuff, maximum stuff)
  print (minimum stuff + maximum stuff)

solve2 invalid = do
  xs <- (map read . words) <$> readFile "input9"
  let stuff = (findCont xs invalid)  
  -- print (minimum stuff, maximum stuff)
  print (minimum stuff + maximum stuff)

--------------------------------------------------------------------------------

main = do
  sol1 <- solve1
  solve2 sol1
