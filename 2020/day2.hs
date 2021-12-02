
{-# LANGUAGE BangPatterns #-}
import Data.List.Split

--------------------------------------------------------------------------------
-- part 1

valid ((!n1,!n2),!c,str) = (n >= n1 && n <= n2) where
  n = length (filter (==c) str)

parse :: String -> ((Int,Int),Char,String)
parse text = case words text of
  [w1,w2,w3] -> case w2 of 
    (ch:':':[]) -> case splitOn "-" w1 of
      [xx,yy] -> ((read xx,read yy),ch,w3)

--------------------------------------------------------------------------------
-- part 2

valid2 ((!n1,!n2),!ch,str) = a || b where
  a = (x == ch) && (y /= ch)
  b = (y == ch) && (x /= ch)
  x = str!!(n1-1)
  y = str!!(n2-1)  

--------------------------------------------------------------------------------

test = 
  [ "1-3 a: abcde"
  , "1-3 b: cdefg"
  , "2-9 c: ccccccccc"
  ]

{-
main1 = do
  let xs = map parse test
  mapM_ print xs
  print $ map valid xs
-}

main2 = do
  let xs = map parse test
  mapM_ print xs
  print $ map valid2 xs

--------------------------------------------------------------------------------

main = do
  ls <- lines <$> readFile "input2"
  -- mapM_ print [ (p, valid2 p)  | l <- ls , let p = parse l ]
  print $ length $ filter valid  $ map parse ls
  print $ length $ filter valid2 $ map parse ls

