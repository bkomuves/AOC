
{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Set        as Set ; import Data.Set        (Set)
import qualified Data.Map.Strict as Map ; import Data.Map.Strict (Map)

import Data.List
import Data.List.Split

{-
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
-}

shiny = "shiny_gold"

cat a1 a2 = a1 ++ "_" ++ a2

parse1 :: String -> (String,[(String,Int)])
parse1 line = case words line of
  (a1:a2:"bags":"contain":rest) -> case unwords rest of
    "no other bags." -> (cat a1 a2 , [])
    what             -> case splitOn ", " (init what) of 
      parts -> (cat a1 a2, map parsePart parts)

parsePart :: String -> (String,Int)
parsePart part = case words part of
  (num:a1:a2:"bags":[]) -> (cat a1 a2, read num)
  (num:a1:a2:"bag" :[]) -> (cat a1 a2, read num)

type Node = String
type Edge = Int

data Graph = Graph
  { _children :: Map Node [(Node,Int)]     -- contains
  , _parents  :: Map Node [(Node,Int)]
  }
  deriving Show

graphFromList :: [(Node,(Node,Edge))] -> Graph
graphFromList list = Graph children parents where
  children = buildMap (:[]) (:) [ (p,(c,e)) | (p,(c,e)) <- list ]
  parents  = buildMap (:[]) (:) [ (c,(p,e)) | (p,(c,e)) <- list ]

buildMap :: Ord k => (a -> b) -> (a -> b -> b) -> [(k,a)] -> Map k b
buildMap f g xys = foldl' insert Map.empty xys where
  insert !old (!k,!x) = Map.alter (h x) k old
  -- h :: a -> Maybe b -> Maybe b
  h x Nothing  = Just (f x  )
  h x (Just y) = Just (g x y)

dfsBackwards :: Graph -> Node -> Set Node
dfsBackwards graph start = execState (go start) Set.empty where
  go node = do
    visited <- get
    unless (Set.member node visited) $ do
      put (Set.insert node visited)
      let next = map fst $ Map.findWithDefault [] node (_parents graph)
      forM_ next go

dfsForwardCount :: Graph -> Node -> Int
dfsForwardCount graph start = go start where
  go node = 1 + sum [ n * go c | (c,n) <- next ] where
    next = Map.findWithDefault [] node (_children graph)

main = do
  input <- readFile "input7"
  let desc = map parse1 $ lines input
  -- mapM_ print desc
  let graph = graphFromList [ (p,ce) | (p,ces) <- desc , ce <- ces ]
  let set = Set.delete shiny $ dfsBackwards graph shiny 
  -- print set
  putStrLn $ "part1 = " ++ show (Set.size set)
  putStrLn $ "part2 = " ++ show (dfsForwardCount graph shiny - 1)
