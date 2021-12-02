
{-# LANGUAGE BangPatterns #-}

import Data.Array
import Data.List
import Control.Monad
import Data.Maybe

is_empty '.' = True
is_empty _   = False

seat 'L' = True
seat '#' = True
seat _   = False

occupied '#' = True
occupied _  = False

occupied_cnt '#' = 1
occupied_cnt _   = 0

type Pos = (Int,Int)

type State = Array Pos Char

step :: State -> State
step !old = new where
  new = accumArray (flip const) '.' bnds [ ( (i,j), f (i,j) ) | i<-[1..n] , j<-[1..m] ]
  f !ij = case old ! ij of
            'L' -> if occ_cnt == 0 then '#' else 'L'
            '#' -> if occ_cnt >= 4 then 'L' else '#'
            c   -> c
          where
            occ_cnt = foldl' (+) 0 [ occupied_cnt (old!p) | p <- neighs ij ]

  bnds@((0,0),(n1,m1)) = bounds old
  n = n1-1
  m = m1-1

step_v2 :: Array Pos [Pos] -> State -> State
step_v2 !precalc !old = new where
  new = accumArray (flip const) '.' bnds [ ( (i,j), f (i,j) ) | i<-[1..n] , j<-[1..m] ]
  f !ij = case old ! ij of
            'L' -> if occ_cnt == 0 then '#' else 'L'
            '#' -> if occ_cnt >= 5 then 'L' else '#'
            c   -> c
          where
            occ_cnt = foldl' (+) 0 [ occupied_cnt (old!p) | p <- precalc ! ij ]

  bnds@((0,0),(n1,m1)) = bounds old
  n = n1-1
  m = m1-1

steps :: State -> State
steps old = let new = step old in if old == new then old else steps new

steps_v2 :: State -> State
steps_v2 old0 = go old0 where
  !table = mk_neigh_table old0
  go !old = let new = step_v2 table old in if old == new then old else go new

neighs :: Pos -> [Pos]
neighs (i,j) = [ (i+y,j+x) | x<-[-1..1] , y<-[-1..1] , x/=0 || y/=0 ]

dirs :: [Pos]
dirs = [ (x,y) | x<-[-1..1] , y<-[-1..1] , x/=0 || y/=0 ]

inbound :: State -> Pos -> Bool
inbound !arr (!i,!j) = j > x1 && j < x2 && i > y1 && i < y2 where
  ((!y1,!x1),(!y2,!x2)) = bounds arr

plus :: Pos -> Pos -> Pos
plus (a,b) (c,d) = (a+c,b+d)

ray :: Pos -> State -> Pos -> [Pos]
ray !dir !arr !pos 
  = takeWhile (inbound arr)
  $ tail
  $ scanl plus pos (repeat dir)

ray_neigh :: Pos -> State -> Pos -> Maybe Pos
ray_neigh !dir !arr !pos = case dropWhile (\p -> is_empty (arr!p)) (ray dir arr pos) of
  []     -> Nothing
  (p:_)  -> Just p

neighs_v2 :: State -> Pos -> [Pos]
neighs_v2 !arr !pos = catMaybes [ p | dir <- dirs , let !p = ray_neigh dir arr pos ]

mk_neigh_table :: State -> Array Pos [Pos]
mk_neigh_table table = posarr where
  posarr = array ((1,1),(n,m)) [ ( (i,j) , neighs_v2 table (i,j) ) | i<-[1..n] , j<-[1..m] ]
  bnds@((0,0),(n1,m1)) = bounds table
  n = n1-1
  m = m1-1

printState :: State -> IO ()
printState arr = do
  putStrLn "\n-----------------"
  let ((0,0),(n1,m1)) = bounds arr
  forM_ [0..n1] $ \i -> putStrLn [ arr!(i,j) | j<-[0..m1] ]

load :: FilePath -> IO State
load fname = do
  ls <- lines <$> readFile fname
  let n = length ls
  let m = length (head ls)
  return $ accumArray (flip const) '.' ((0,0),(n+1,m+1))
    [ ((i,j),c) | (i,ln) <- zip [1..] ls , (j,c) <- zip [1..] ln ]

main = do
  arr <- load "input11"
  -- arr <- load "test11"
  let precalc  = mk_neigh_table arr
  printState arr
  printState (step_v2 precalc arr)
  -- mapM_ print (assocs precalc)
  let final = steps_v2 arr
  -- printState final
  print $ sum $ map occupied_cnt $ elems final
