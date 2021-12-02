
import qualified Data.Map as Map ; import Data.Map (Map)

import Data.List
import Data.List.Split

data Rule r
  = Chr !Char
  | Alt ![Rule r]
  | Seq ![r]
  deriving Show

parseIntRule :: String -> (Int, Rule Int)
parseIntRule str = case splitOn ": " str of
  [nn, rest] -> (read nn, parseRule_ rest) 

parseRule_ :: String -> Rule Int
parseRule_ what = case what of
  ['"',c,'"'] -> Chr c
  _     -> case splitOn " | " what of
    [x]   -> Seq (map read $ words x)
    list  -> Alt (map parseRule_ list)

parseMessage = id

load fn = do
  ls <- lines <$> readFile fn
  case splitWhen null ls of
    [rules,messages] -> return
      ( Map.fromList (map parseIntRule rules) 
      , map parseMessage messages
      )

match :: Map Int (Rule Int) -> String -> Bool
match ruleTable str = case parse' ruleTable str of
  (b,rem) -> b && null rem 

parse' :: Map Int (Rule Int) -> String -> (Bool,String)
parse' ruleTable = goN 0 where

  goN k str = case Map.lookup k ruleTable of 
    Just what -> goRule what str

  goRule rule []         = (False,[])
  goRule rule str@(x:xs) = case rule of
    Chr c  -> (x == c, xs) 
    Seq ks -> goSeq ks str
    Alt rs -> goAlt rs str 

  goSeq []     str = (True,str)
  goSeq (k:ks) str = case goN k str of
    (ok,rest) -> if ok then goSeq ks rest else (ok,rest)

  goAlt []     str = (False,str)
  goAlt (k:ks) str = case goRule k str of
    (ok,rest) -> if ok then (ok,rest) else (goAlt ks str)

--------------------------------------------------------------------------------

matchV2 :: Map Int (Rule Int) -> String -> Bool
matchV2 ruleTable str = [] `elem` (parseV2 ruleTable str)

parseV2 :: Map Int (Rule Int) -> String -> [String]
parseV2 ruleTable = goN 0 where

  goN k str = case Map.lookup k ruleTable of 
    Just what -> goRule what str

  goRule rule []         = []
  goRule rule str@(x:xs) = case rule of
    Chr c  -> if (x == c) then [xs] else [] 
    Seq ks -> goSeq ks str
    Alt rs -> goAlt rs str 

  goSeq []     str = [str]
  goSeq (k:ks) str = case goN k str of
    xs -> concatMap (goSeq ks) xs

  goAlt [] str = []
  goAlt ks str = concat [ goRule k str | k <- ks ]

main = do
  -- (rules,msg) <- load "test19b"
  (rules,msg) <- load "input19b"
  print rules
  let result = [ (matchV2 rules m , m) | m <- msg ]
  mapM_ print result
  print $ sum [ 1 | (True,_) <- result ]
  
