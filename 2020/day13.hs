
{-

this solution uses a bit of cheating for part 2:

instead fully solving it in Haskell, the Haskell code outputs Mathematica code.
this is because I was way too lazy to implement the Chinese remainder theorem.

-}

import Data.List

--------------------------------------------------------------------------------

type Bus = Int  -- bus id
type TS  = Int  -- timestamp

type Problem = (TS,[Bus])

--------------------------------------------------------------------------------
-- part 1

solve1 :: Problem -> IO ()
solve1 (start,buses0) = 
  do
    let sol@(t,bus,diff,multiplied) = head $ head sols
    putStrLn $ "earliest bus = " ++ show sol 
    putStrLn $ "answer = " ++ show multiplied

  where
    buses = filter (>0) buses0
    sols = dropWhile null 
      [ [ (t,bus,t-start,bus*(t-start)) | bus <- buses, mod t bus == 0 ]  
      | t <- [start..] 
      ]

--------------------------------------------------------------------------------
-- part2

solve2_naive :: Problem -> IO ()
solve2_naive (start,buses0) = 
  do
    print $ map fst $ buses_offsets
    print buses_offsets
    print (head sol) 
  where
    buses_offsets = filter (\(bus,ofs) -> bus > 0) $ zip buses0 [0..] 
    sol = 
      [ t 
      | t <- [start..] 
      , let list = [ mod (t+ofs) bus | (bus,ofs) <- buses_offsets ]  
      , all (==0) list
      ]

solve2 :: Problem -> IO ()
solve2 (start,buses0) = 
  do
    putStrLn $ "(* " ++ show (map fst $ buses_offsets) ++ " *)"
    putStrLn $ "(* " ++ show buses_offsets ++ " *)"
    putStrLn $ "rs = " ++ mlist (map snd buses_offsets)  
    putStrLn $ "ms = " ++ mlist (map fst buses_offsets) 
    putStrLn $ "ChineseRemainder[ Map[Minus,rs] , ms ]"
  where
    mlist xs = "{" ++ intercalate "," (map show xs) ++ "}"
    buses_offsets = filter (\(bus,ofs) -> bus > 0) $ zip buses0 [0..] 

--------------------------------------------------------------------------------

test = "939\n7,13,x,x,59,x,31,19"

parse :: String -> Problem
parse text = (read l1 , read s) where
  [l1,l2] = lines text
  f 'x' = '0'
  f c   = c 
  s = "[" ++ map f l2 ++ "]"

load :: FilePath -> IO Problem
load fn = parse <$> readFile fn

--------------------------------------------------------------------------------

main1 :: Problem -> IO ()
main1 problem = do
  putStrLn "\npart 1\n"
  -- solve1 $ parse test
  solve1 problem

main2 :: Problem -> IO ()
main2 problem = do
  putStrLn "\npart 2\n(with some cheating: paste the output below into Mathematica)\n"
  -- solve2_naive $ parse test
  -- solve2       $ parse test
  solve2 problem

main = do
  p <- load "input13"
  main1 p
  main2 p

