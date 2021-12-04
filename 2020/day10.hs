
import Data.List
import Data.Array

--------------------------------------------------------------------------------

diffs (a:b:rest) = (b-a) : diffs (b:rest)
diffs [a] = []
diffs []  = []

histo :: [Int] -> [(Int,Int)]
histo = map (\xs -> (head xs, length xs)) . group . sort 

solve1 :: [Int] -> [(Int,Int)]
solve1 = histo . diffs . initialize

addmax :: [Int] -> [Int]
addmax xs = (maximum xs + 3) : xs

initialize :: [Int] -> [Int]
initialize = (0:) . sort . addmax  

--------------------------------------------------------------------------------

test_a = [16,10,15,5,1,11,7,19,6,12,4] :: [Int]

stuff_and_rest :: Int -> [Int] -> [[Int]]
stuff_and_rest threshold = go where
  go (x:xs) = if x <= threshold then (x:xs) : go xs else []
  go [] = []

possibleSlow :: [Int] -> Integer
possibleSlow [n] = 1
possibleSlow (a:xs) = foldl' (+) 0 [ possibleSlow ls | ls <- stuff_and_rest (a+3) xs ] 

possibleFast :: [Int] -> Integer
possibleFast xs = possibleArr xs ! 0

possibleArr :: [Int] -> Array Int Integer
possibleArr input = sol where
  len   = length input
  inp   = listArray (0,len-1) input
  sol   = array (0,len-1) [ (i,worker i) | i<-[0..len-1] ]

  worker :: Int -> Integer
  worker i 
    | i == len-1  = 1
    | otherwise   = foldl' (+) 0 [ sol ! k | k<-[i+1..len-1] , inp!k <= (inp!i + 3) ] 

--------------------------------------------------------------------------------

main :: IO ()
main = do
  xs <- (map read . words) <$> readFile "input10" :: IO [Int]
  let hist = solve1 xs
  print hist
  putStrLn $ "part 1: " ++ show ((*) <$> lookup 1 hist <*> lookup 3 hist)
  putStrLn $ "part 2: " ++ show (possibleFast $ initialize xs)
