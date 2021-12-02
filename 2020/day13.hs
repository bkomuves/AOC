
import Data.List

test = "939\n7,13,x,x,59,x,31,19"

type Problem = (Int,[Int])

solve1 :: Problem -> IO ()
solve1 (start,buses0) = print (head sol) where
  buses = filter (>0) buses0
  sol = dropWhile null 
    [ [ (t,bus,t-start,bus*(t-start)) | bus <- buses, mod t bus == 0 ]  
    | t <- [start..] 
    ]

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
    print $ map fst $ buses_offsets
    print buses_offsets
    putStrLn $ "rs = " ++ mlist (map snd buses_offsets)  
    putStrLn $ "ms = " ++ mlist (map fst buses_offsets) 
    putStrLn $ "ChineseRemainder[ Map[Minus,rs] , ms ]"
  where
    mlist xs = "{" ++ intercalate "," (map show xs) ++ "}"
    buses_offsets = filter (\(bus,ofs) -> bus > 0) $ zip buses0 [0..] 

parse :: String -> Problem
parse text = (read l1 , read s) where
  [l1,l2] = lines text
  f 'x' = '0'
  f c   = c 
  s = "[" ++ map f l2 ++ "]"

load :: FilePath -> IO Problem
load fn = parse <$> readFile fn

main1 = do
  solve1 $ parse test
  p <- load "input13"
  solve1 p

main2 = do
--  solve2_naive $ parse test
  solve2 $ parse test
  p <- load "input13"
  print p
  solve2 p

