
import Data.Array

--------------------------------------------------------------------------------

type Input = ((Int,Int),Array (Int,Int) Char)

load :: IO Input
load = do
  ls0 <- lines <$> readFile "input3"
  let m = length (head ls0)
  let ls = filter (\xs -> length xs == m) ls0
  let n = length ls
  -- print (n,m)
  let arr = array ((0,0),(n-1,m-1)) [ ((i,j),(ls!!i)!!j) | i<-[0..n-1] , j<-[0..m-1]]
  return ((n,m),arr)

--------------------------------------------------------------------------------

f :: Char -> Int
f '#' = 1
f '.' = 0

g :: Char -> Char
g '#' = 'X'
g '.' = 'O'

--------------------------------------------------------------------------------
-- part 1

part1 :: Input -> IO ()
part1 ((n,m),arr) = do
  putStrLn "\npart 1:"
  let selected = [ arr ! ( i , mod (3*i) m ) | i<-[1..n-1] ]
  let treecnt = sum (map f selected)
  -- print $ map g selected
  putStrLn $ "number of trees = " ++ show treecnt

--------------------------------------------------------------------------------
-- part 2

type Dir = (Int,Int)

directions :: [Dir]
directions = [(1,1),(3,1),(5,1),(7,1),(1,2)]

part2 :: Input -> IO ()
part2 ((n,m),arr) = do
  putStrLn "\npart 2:"
  let poslist (x,y) = takeWhile (\xy -> fst xy < n)
                    $ [ (k*y , k*x) | k <- [1..] ]
  let selected (x,y) = [ arr ! (i , mod j m) | (i,j) <- poslist (x,y) ]
  let treecnt xy = sum (map f $ selected xy)
  let counts = map treecnt directions
  putStrLn $ "counts  = " ++ show counts
  putStrLn $ "product = " ++ show (product counts)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- load
  part1 input
  part2 input

