
{-# LANGUAGE BangPatterns #-}

import Data.Array

--------------------------------------------------------------------------------

margin :: Int
margin = 6

iter :: Int -> (a -> a) -> (a -> a)
iter 0 f = id
iter n f = iter (n-1) f . f

--------------------------------------------------------------------------------
-- part 1

type Array3D = Array (Int,Int,Int) Bool

step1 :: Array3D -> Array3D
step1 !old = new where

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

load1 :: FilePath -> IO Array3D
load1 fn = do
  ls <- lines <$> readFile fn
  let n = length ls
      m = length (head ls)
  let arr = accumArray (flip const) False ((1-margin,1-margin,1-margin),(n+margin,m+margin,1+margin))
              [ ((i,j,1), c2b c) | (i,ln) <- zip [1..] ls , (j,c) <- zip [1..] ln ]
  return arr
  where
    c2b '.' = False
    c2b '#' = True

part1 :: IO ()
part1 = do
  arr <- load1 "input17"
  let final = iter 6 step1 arr
  let answer = length $ filter id $ elems final 
  putStrLn $ "answer to part 1 = " ++ show answer
  
--------------------------------------------------------------------------------

type Array4D = Array (Int,Int,Int,Int) Bool

step2 :: Array4D -> Array4D 
step2 !old = new where

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

load2 :: FilePath -> IO Array4D
load2 fn = do
  ls <- lines <$> readFile fn
  let n = length ls
      m = length (head ls)
  let arr = accumArray (flip const) False ((1-margin,1-margin,1-margin,1-margin),(n+margin,m+margin,1+margin,1+margin))
              [ ((i,j,1,1), c2b c) | (i,ln) <- zip [1..] ls , (j,c) <- zip [1..] ln ]
  return arr
  where
    c2b '.' = False
    c2b '#' = True

part2 :: IO ()
part2 = do
  arr <- load2 "input17"
  let final = iter 6 step2 arr
  let answer = length $ filter id $ elems final 
  putStrLn $ "answer to part 2 = " ++ show answer

--------------------------------------------------------------------------------

main :: IO ()
main = do
  part1
  part2
