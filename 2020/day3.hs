
import Data.Array

f :: Char -> Int
f '#' = 1
f '.' = 0

g '#' = 'X'
g '.' = 'O'

load = do
  -- ls0 <- lines <$> readFile "test3" 
  ls0 <- lines <$> readFile "input3"
  let m = length (head ls0)
  let ls = filter (\xs -> length xs == m) ls0
  let n = length ls
  print (n,m)
  let arr = array ((0,0),(n-1,m-1)) [ ((i,j),(ls!!i)!!j) | i<-[0..n-1] , j<-[0..m-1]]
  return ((n,m),arr)

main1 = do
  ((n,m),arr) <- load
  let selected = [ arr ! ( i , mod (3*i) m ) | i<-[1..n-1] ]
  let treecnt = sum (map f selected)
  print $ map g selected
  print treecnt

dirs = [(1,1),(3,1),(5,1),(7,1),(1,2)]

main2 = do
  ((n,m),arr) <- load
  let poslist (x,y) = takeWhile (\xy -> fst xy < n)
                    $ [ (k*y , k*x) | k <- [1..] ]
  let selected (x,y) = [ arr ! (i , mod j m) | (i,j) <- poslist (x,y) ]
  let treecnt xy = sum (map f $ selected xy)
  let counts = map treecnt dirs
  print counts
  print $ product counts