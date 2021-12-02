
{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map
import Data.Map (Map)

input = [1,2,16,19,18,0]

ex1 = [0,3,6]

type Turn = Int

type State = Map Turn (Int,Int)

run :: [Int] -> [Int]
run input = go where
  ini = Map.fromList $ [ (x,(j,-1)) | (j,x) <- zip [1..] input ] 
  go = input ++ loop (length ini+1) (last input) ini where

  loop !turn !last !state = spoken : loop (turn+1) spoken state' where
    (spoken,state') = step turn last state

runN :: Int -> [Int] -> Int
runN count input 
  | count <= length input = input !! (count-1)
  | otherwise = res 
  where
  ini = Map.fromList $ [ (x,(j,-1)) | (j,x) <- zip [1..] input ] 
  res = loop (length ini+1) (last input) ini where

  loop !turn !last !state = if turn == count then spoken else loop (turn+1) spoken state' where
    (spoken,state') = step turn last state

step :: Turn -> Int -> State -> (Int,State)
step !turn !last !oldstate = (spoken,newstate) where
  spoken = case Map.lookup last oldstate of
    Just (age1,age2) -> if age2 == -1 then 0 else age1-age2 
  newstate = case Map.lookup spoken oldstate of
    Just (age1,age2) -> Map.insert spoken (turn,age1) oldstate
    Nothing          -> Map.insert spoken (turn,-1  ) oldstate

main1 = do
  print $ take 10 $ run ex1
  print $ (run ex1 !! 2019)

  print $ (run [1,3,2] !! 2019)  
  print $ (run [2,1,3] !! 2019)   
  print $ (run [1,2,3] !! 2019)  
  print $ (run [2,3,1] !! 2019)   
  print $ (run [3,2,1] !! 2019)  
  print $ (run [3,1,2] !! 2019)   
   
  print $ run input !! 2019

main = do
  print $ take 5000 $ run ex1
  --print $ runN 2020 ex1   
  --print $ runN 2020 [3,1,2]
  let n = 30000000
  print n
  -- print $ runN n input 
  --print $ runN n ex1   
  print "fuck"