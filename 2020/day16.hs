
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Ord

--------------------------------------------------------------------------------

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y

type Ticket = [Int]

type Cond  = (Int,Int)
type CondPair = (Cond,Cond)

data Problem = Problem
  { _conditions :: [(String,CondPair)]
  , _my      :: Ticket
  , _tickets :: [Ticket]
  }
  deriving Show

readIntList :: String -> [Int]
readIntList str = read ("[" ++ str ++ "]")

parseCond1 :: String -> Cond
parseCond1 str = case splitOn "-" str of
  [a,b] -> (read a, read b)

parseCondLine :: String -> (String,CondPair)
parseCondLine str = case splitOn ": " str of
  [name,what] -> case splitOn " or " what of
    [one,two] -> (name,(parseCond1 one, parseCond1 two))

load :: FilePath -> IO Problem
load fn = do
  ls <- lines <$> readFile fn
  let [para1,para2,para3] = splitWhen null ls
      conds = map parseCondLine para1
      my      = case para2 of [ "your ticket:"    , stuff ] -> readIntList stuff
      tickets = case para3 of ( "nearby tickets:" : rest  ) -> map readIntList rest 
  return $ Problem conds my tickets

--------------------------------------------------------------------------------
-- part 1

checkCond :: Int -> Cond -> Bool
checkCond x (a,b) = x >= a && x <= b

checkCondPair :: Int -> CondPair -> Bool
checkCondPair x (p,q) = checkCond x p || checkCond x q

checkCondsAny :: Int -> [CondPair] -> Bool
checkCondsAny x cs = any (checkCondPair x) cs

solve1 :: Problem -> [Int]
solve1 (Problem conds _ tickets) = filter (\x -> not $ checkCondsAny x (map snd conds)) (concat tickets)

--------------------------------------------------------------------------------
-- part 2

possibleNames :: [(a,CondPair)] -> [Int] -> [a]
possibleNames conds numbers = [ name | (name,condp) <- conds , all (\x -> checkCondPair x condp) numbers ]

mbSingleton :: [a] -> Maybe a
mbSingleton [x] = Just x
mbSingleton _   = Nothing

solve2 :: [(String,[Int])] -> [(String,Int)]
solve2 = worker where
  worker [] = []
  worker stuff = 
    case catMaybes [ (name,) <$> mbSingleton list | (name,list) <- stuff ] of
      ((name,j) : _) -> (name,j) : worker next where
        next = [ (n,l') | (n,l) <- stuff , n /= name , let l' = l \\ [j] ]

{-
-- was used for debugging 
solve2_IO :: [(String,[Int])] -> IO [(String,Int)]
solve2_IO = worker where
  worker [] = return []
  worker stuff = do
    print stuff
    let mbs = [ (name,) <$> mbSingleton list | (name,list) <- stuff ]
    print mbs
    case catMaybes mbs of
      ((name,j) : _) -> ((name,j) :) <$> worker next where
        next = [ (n,l') | (n,l) <- stuff , n /= name , let l' = l \\ [j] ]
-}

--------------------------------------------------------------------------------

part1 :: Problem -> IO ()
part1 problem@(Problem conds my tickets) = do
  putStrLn "\npart 1"
  let bad = solve1 problem
  putStrLn $ "bad tickets      = " ++ show bad
  putStrLn $ "answer to part 1 = " ++ show (sum bad)

part2 :: Problem -> IO ()
part2 problem@(Problem conds my tickets) = do
  putStrLn "\npart 2"

  let bad = solve1 problem
  let goodtickets = [ ticket | ticket <- tickets , all (\x -> checkCondsAny x (map snd conds)) ticket ]
  let tr = transpose goodtickets
  let candidates = [ (name,idx) | (idx,column) <- zip [0..] tr , name <- possibleNames conds column ]
  let named_cand = map (\xs -> (fst (head xs) , map snd xs))
                 $ groupBy (equating fst) $ sortBy (comparing fst) candidates
  let sol = solve2 named_cand
  -- mapM_ print sol

  let idxs = [ j | (name,j) <- sol , isPrefixOf "departure" name ]
  putStrLn $ "departure indices = " ++ show idxs
  putStrLn $ "on my ticket      = " ++ show [ my !! j | j <- idxs ]
  putStrLn $ "answer to part 2  = " ++ show (product [ my !! j | j <- idxs ])

  -- putStrLn "sanity check:"
  -- forM_ sol $ \(name,idx) -> do
  --   let Just condpair = lookup name conds
  --   let column = tr !! idx
  --   putStrLn $ show (name,idx,condpair) ++ " -> " ++ show (and [ checkCondPair x condpair | x <- column ])

main :: IO ()
main = do
  problem <- load "input16"
  part1 problem
  part2 problem

