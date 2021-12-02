
import Data.Ord
import Data.List
import Data.List.Split

import qualified Data.Set        as Set ; import Data.Set        (Set)
import qualified Data.Map.Strict as Map ; import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

type Line = ([String],[String])

parseLine :: String -> Line
parseLine line = case splitOn " (contains " line of
  [ings,allergs] -> case splitOn ", " (init allergs) of
    alls -> (words ings, alls)

load :: FilePath -> IO [Line]
load fn = do
  ls <- lines <$> readFile fn
  return $ map parseLine ls

intersections :: Ord a => [Set a] -> Set a
intersections = foldl1 Set.intersection

possible :: [Line] -> Map String (Set String)
possible stuff = Map.fromList (map worker $ Set.toList allSet) where
  -- ingSet = Set.fromList (concatMap fst stuff)
  allSet = Set.fromList (concatMap snd stuff)
  worker allerg = (allerg, set) where
    set = intersections [ Set.fromList ings | (ings,alls) <- stuff , elem allerg alls ]

isSingleton :: Ord a => Set a -> Bool
isSingleton set = case Set.minView set of
  Just (x,rest) -> Set.null rest
  _             -> False

mbSingleton :: Ord a => Set a -> Maybe a
mbSingleton set = case Set.minView set of
  Just (x,rest) -> if Set.null rest then Just x else Nothing
  _             -> Nothing

solve :: Ord a => Map a (Set a) -> [(a,a)]
solve table = if Map.null table then [] else case find (isSingleton . snd) (Map.toList table) of
  Nothing -> error "solve"
  Just (allerg,singl) -> case mbSingleton singl of
    Nothing  -> error "fatal"
    Just ing -> (allerg,ing) : solve (deleteIng ing $ Map.delete allerg table)
  where
    deleteIng ing = Map.map (Set.delete ing)

main = do 
  -- stuff <- load "test21"
  stuff <- load "input21"
  -- mapM_ print stuff

  let ingSet = Set.fromList (concatMap fst stuff)
  let allSet = Set.fromList (concatMap snd stuff)
  -- print ingSet
  -- print allSet

  let possibles = possible stuff
  mapM_ print $ Map.toList $ possibles

  let bad_candidates = Set.unions $ Map.elems possibles
  let safe = Set.difference ingSet bad_candidates
  print safe
  print $ Set.size safe
  
  let count1 = sum [ sum [ 1 | (ings,_) <- stuff , elem ing ings ] | ing <- Set.toList safe ]
  print count1

  let sol = sortBy (comparing fst) $ solve possibles
  print sol
  putStrLn $ intercalate "," $ map snd sol
