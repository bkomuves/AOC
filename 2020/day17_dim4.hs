
{-# LANGUAGE BangPatterns #-}

import Data.Array

margin = 6

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = iter (n-1) f . f

step :: Array (Int,Int,Int,Int) Bool -> Array (Int,Int,Int,Int) Bool 
step old = new where

  f !i !j !k !l = 
    case old!(i,j,k,l) of
      False -> (cnt == 3)
      True  -> (cnt == 2) || (cnt == 3)
    where
      cnt = length $ filter id [ old!(y,x,z,w) | (y,x,z,w) <- neighs (i,j,k,l) ]

  new = accumArray (flip const) False bnds
    [ ((i,j,k,l), f i j k l) | i<-[ymin..ymax] , j<-[xmin..xmax] , k<-[zmin..zmax] , l<-[wmin..wmax] ]

  bnds@((ymin,xmin,zmin,wmin),(ymax,xmax,zmax,wmax)) = bounds old
  neighs (y,x,z,w) = [ (y',x',z',w') 
                   | a<-[-1..1] , b<-[-1..1] , c<-[-1..1] , d<-[-1..1] 
                   , a/=0 || b/=0 || c/=0 || d/=0
                   , let y' = y+b , let x' = x+a , let z' = z+c , let w' = w+d
                   , x' >= xmin , x' <= xmax 
                   , y' >= ymin , y' <= ymax 
                   , z' >= zmin , z' <= zmax 
                   , w' >= wmin , w' <= wmax 
                   ]

printArr arr = do
  let bnds@((ymin,xmin,zmin,wmin),(ymax,xmax,zmax,wmax)) = bounds arr
  return ()

load fn = do
  ls <- lines <$> readFile fn
  let n = length ls
      m = length (head ls)
  let arr = accumArray (flip const) False ((1-margin,1-margin,1-margin,1-margin),(n+margin,m+margin,1+margin,1+margin))
              [ ((i,j,1,1), c2b c) | (i,ln) <- zip [1..] ls , (j,c) <- zip [1..] ln ]
  return arr
  where
    c2b '.' = False
    c2b '#' = True

main = do
  arr <- load "input17"
  let final = iter 6 step arr
  print $ length $ filter id $ elems final 
