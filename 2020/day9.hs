
{-# LANGUAGE BangPatterns #-}

import Data.Array
import Data.List

check' :: Int -> [Int] -> Maybe Int
check' n list = go (take n list) (drop n list) where
  go _ [] = Nothing
  go !preamble (!this:rest) = 
    if elem this sumtwo
      then go preamble' rest
      else Just this
    where
      sumtwo    = [ a + b | a <- preamble , b <- preamble , a /= b ]
      preamble' = tail preamble ++ [this]

findCont :: [Int] -> Int -> [Int]
findCont list tgt = head [ [ arr!k | k<-[a+1..b] ] | a<-[0..n-2] , b<-[2..n] , cumsum!b - cumsum!a == tgt ]  where
  arr    = listArray (1,n) $ list
  cumsum = listArray (0,n) $ scanl (+) 0 list
  n = length list

main1 = do
  xs <- (map read . words) <$> readFile "test9"
  print (check' 5 xs)
  xs <- (map read . words) <$> readFile "input9"
  print (check' 25 xs)  

main2 = do
  xs <- (map read . words) <$> readFile "test9"
  let stuff = (findCont xs 127)
  print (stuff)
  print (sum stuff)
  print (minimum stuff, maximum stuff, minimum stuff + maximum stuff)

  xs <- (map read . words) <$> readFile "input9"
  let stuff = (findCont xs 1124361034)  
  print (sum stuff)
  print (minimum stuff, maximum stuff, minimum stuff + maximum stuff)
