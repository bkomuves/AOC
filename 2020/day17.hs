
{-# LANGUAGE BangPatterns #-}

import Data.Array

margin = 6

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = iter (n-1) f . f

step :: Array (Int,Int,Int) Bool -> Array (Int,Int,Int) Bool 
step old = new where

  f !i !j !k = 
    case old!(i,j,k) of
      False -> (cnt == 3)
      True  -> (cnt == 2) || (cnt == 3)
    where
      cnt = length $ filter id [ old!(y,x,z) | (y,x,z) <- neighs (i,j,k) ]

  new = accumArray (flip const) False bnds
    [ ((i,j,k), f i j k) | i<-[ymin..ymax] , j<-[xmin..xmax] , k<-[zmin..zmax] ]

  bnds@((ymin,xmin,zmin),(ymax,xmax,zmax)) = bounds old
  neighs (y,x,z) = [ (y',x',z') 
                 | a<-[-1..1] , b<-[-1..1] , c<-[-1..1] 
                 , a/=0 || b/=0 || c/=0
                 , let y' = y+b , let x' = x+a , let z' = z+c 
                 , x' >= xmin , x' <= xmax 
                 , y' >= ymin , y' <= ymax 
                 , z' >= zmin , z' <= zmax 
                 ]

printArr arr = do
  let bnds@((ymin,xmin,zmin),(ymax,xmax,zmax)) = bounds arr
  return ()

load fn = do
  ls <- lines <$> readFile fn
  let n = length ls
      m = length (head ls)
  let arr = accumArray (flip const) False ((1-margin,1-margin,1-margin),(n+margin,m+margin,1+margin))
              [ ((i,j,1), c2b c) | (i,ln) <- zip [1..] ls , (j,c) <- zip [1..] ln ]
  return arr
  where
    c2b '.' = False
    c2b '#' = True

main = do
  arr <- load "input17"
  let final = iter 6 step arr
  print $ length $ filter id $ elems final 
