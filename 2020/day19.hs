
import qualified Data.Map as Map ; import Data.Map (Map)

import Data.List
import Data.List.Split

--------------------------------------------------------------------------------

data Rule r
  = Chr !Char
  | Alt ![Rule r]
  | Seq ![r]
  deriving (Eq,Show)

parseIntRule :: String -> (Int, Rule Int)
parseIntRule str = case splitOn ": " str of
  [nn, rest] -> (read nn, parseRule_ rest) 

parseRule_ :: String -> Rule Int
parseRule_ what = case what of
  ['"',c,'"'] -> Chr c
  _     -> case splitOn " | " what of
    [x]   -> Seq (map read $ words x)
    list  -> Alt (map parseRule_ list)

parseMessage :: String -> String
parseMessage = id

load :: FilePath -> IO (Map Int (Rule Int), [String])
load fn = do
  ls <- lines <$> readFile fn
  case splitWhen null ls of
    [rules,messages] -> return
      ( Map.fromList (map parseIntRule rules) 
      , map parseMessage messages
      )

--------------------------------------------------------------------------------
-- part 1

matchV1 :: Map Int (Rule Int) -> String -> Bool
matchV1 ruleTable str = case parseV1 ruleTable str of
  (b,rem) -> b && null rem 

parseV1 :: Map Int (Rule Int) -> String -> (Bool,String)
parseV1 ruleTable = goN 0 where

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
-- part 2

oldRule8, oldRule11 :: Rule Int
oldRule8  = Seq [42]
oldRule11 = Seq [42,31]

newRule8, newRule11 :: Rule Int
newRule8  = Alt [ Seq [42]    , Seq [42,8]     ]
newRule11 = Alt [ Seq [42,31] , Seq [42,11,31] ]

replaceRules :: Map Int (Rule Int) -> Map Int (Rule Int)
replaceRules old = if (Map.lookup 8 old == Just oldRule8) && (Map.lookup 11 old) == Just (oldRule11)
  then Map.insert 8 newRule8 $ Map.insert 11 newRule11 old
  else error "replaceRules: the old rules #8 and/or #11 do not match the expected ones"

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

--------------------------------------------------------------------------------

part1 :: IO ()
part1 = do
  putStrLn "\npart 1"
  (rules,msg) <- load "input19"
  let result = [ (matchV1 rules m , m) | m <- msg ]
  -- print rules
  -- mapM_ print result
  let answer = sum [ 1 | (True,_) <- result ]
  putStrLn $ "the answer to part 1 = " ++ show answer

part2 :: IO ()
part2  = do
  putStrLn "\npart 2"
  (old_rules,msg) <- load "input19"
  let rules = replaceRules old_rules
  let result = [ (matchV2 rules m , m) | m <- msg ]
  -- print rules
  -- mapM_ print result
  let answer = sum [ 1 | (True,_) <- result ]
  putStrLn $ "the answer to part 2 = " ++ show answer
  
main :: IO ()
main = do
  part1
  part2
