
{-# LANGUAGE Strict #-}

import Data.List.Split

--------------------------------------------------------------------------------

type Input = ((Int,Int),Char,String)

parse :: String -> Input
parse text = case words text of
  [w1,w2,w3] -> case w2 of 
    (ch:':':[]) -> case splitOn "-" w1 of
      [xx,yy] -> ((read xx,read yy),ch,w3)

--------------------------------------------------------------------------------
-- part 1

valid1 :: Input -> Bool
valid1 ((n1,n2),c,str) = (n >= n1 && n <= n2) where
  n = length (filter (==c) str)

--------------------------------------------------------------------------------
-- part 2

valid2 :: Input -> Bool
valid2 ((n1,n2),ch,str) = a || b where
  a = (x == ch) && (y /= ch)
  b = (y == ch) && (x /= ch)
  x = str!!(n1-1)
  y = str!!(n2-1)  

--------------------------------------------------------------------------------

main = do
  ls <- lines <$> readFile "input2"
  print $ length $ filter valid1 $ map parse ls
  print $ length $ filter valid2 $ map parse ls

