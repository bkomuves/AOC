
{-# LANGUAGE Strict, BangPatterns #-}

import Data.List

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.Map.Strict (Map)
import Data.Set        (Set)

iter :: Int -> (a -> a) -> (a -> a)
iter !n f !x = go n x where
  go !n !x 
    | n == 0    = x
    | otherwise = let !y = f x in go (n-1) y

data Dir = E | SE | SW | W | NW | NE deriving (Eq,Show)

allDirs = [E,SE,SW,W,NW,NE]

type Pos = (Int,Int)

sum' :: [Int] -> Int
sum' = foldl' (+) 0 

data Color = Black | White deriving (Eq,Show)

oppoCol Black = White
oppoCol White = Black

dirToXY :: Dir -> Pos
dirToXY d = case d of
  E   -> (1,0 )
  SE  -> (1,-1)
  SW  -> (0,-1)
  W   -> (-1,0)
  NW  -> (-1,1)
  NE  -> (0 ,1)

plus :: Pos -> Pos -> Pos
plus (a,b) (x,y) = (a+x,b+y)

pathToXY :: [Dir] -> Pos
pathToXY dirs = foldl' plus (0,0) (map dirToXY dirs)

parseLine :: String -> [Dir]
parseLine = go where
  go [] = []
  go ('e':    rest) = E : go rest
  go ('s':'e':rest) = SE : go rest
  go ('s':'w':rest) = SW : go rest
  go ('w':    rest) = W : go rest
  go ('n':'e':rest) = NE : go rest
  go ('n':'w':rest) = NW : go rest

load :: FilePath -> IO [[Dir]]
load fn = do
  ls <- lines <$> readFile fn
  return $ map parseLine ls

setColor :: Pos -> Color -> Map Pos Color -> Map Pos Color
setColor pos White table = Map.delete pos       table
setColor pos Black table = Map.insert pos Black table

lkpColor :: Pos -> Map Pos Color -> Color
lkpColor pos table = Map.findWithDefault White pos table

flipColor :: Pos -> Map Pos Color -> Map Pos Color
flipColor pos table = case lkpColor pos table of
  Black -> setColor pos White table
  White -> setColor pos Black table

part1 :: [[Dir]] -> Map Pos Color
part1 = foldl' worker Map.empty where
  worker oldMap path = flipColor (pathToXY path) oldMap

countBlacks :: Map Pos Color -> Int
countBlacks table = sum' [ 1 | col <- Map.elems table , col == Black ]

--------------------------------------------------------------------------------

neighs :: Pos -> [Pos]
neighs pos = [ plus pos (dirToXY dir) | dir <- allDirs ]

neighsAndItself :: Pos -> [Pos]
neighsAndItself pos = pos : [ plus pos (dirToXY dir) | dir <- allDirs ]

tileCandidates :: Map Pos Color -> Set Pos
tileCandidates = Set.fromList . concatMap neighsAndItself . Map.keys

step :: Map Pos Color -> Map Pos Color
step oldtable = foldl' worker oldtable candidates where
  worker table pos = 
    case lkpColor pos table of
      Black -> if cnt == 0 || cnt > 2 then setColor pos White table else table
      White -> if cnt == 2            then setColor pos Black table else table
    where
      cnt = countBlackNeighs pos 

  candidates = Set.toList (tileCandidates oldtable)
  countBlackNeighs pos = sum' [ 1 | q <- neighs pos , lkpColor q oldtable == Black ]

--------------------------------------------------------------------------------

main = do
  input <- load "input24"
  let table = part1 input
  print $ countBlacks table
  print $ countBlacks $ iter 100 step $ table
