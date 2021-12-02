
{-# LANGUAGE BangPatterns #-}

import Data.List.Split
import qualified Data.Set as Set ; import Data.Set (Set)
import qualified Data.Sequence as Seq ; import Data.Sequence ( Seq , (<|) , (|>) , ViewL(..) , ViewR(..) )
import Data.Foldable as F

data Player = Player1 | Player2 deriving Show

data Result = Result Player [Int] deriving Show

play (list1,list2) = go (Seq.fromList list1) (Seq.fromList list2) where
  go !deck1 !deck2 = case (Seq.viewl deck1 , Seq.viewl deck2) of
    (EmptyL  , _      ) -> Result Player2 (F.toList deck2)
    (_       , EmptyL ) -> Result Player1 (F.toList deck1)
    (x :< xs , y :< ys) -> if x > y 
      then go (xs |> x |> y) ys 
      else go xs (ys |> y |> x)

load :: FilePath -> IO ([Int],[Int])
load fn = do
  ls <- lines <$> readFile fn
  case splitWhen null ls of
    [ls1,ls2] -> case ls1 of
      ("Player 1:":deck1) -> case ls2 of
        ("Player 2:":deck2) -> return (map read deck1, map read deck2)


play2 (list1,list2) = go (Set.empty) (Seq.fromList list1) (Seq.fromList list2) where

  go !previous !deck1 !deck2 = do
    -- print (F.toList deck1)
    -- print (F.toList deck2)
    if Set.member (deck1,deck2) previous
      then do
        -- putStrLn "player #1 instawin!"
        return $ Result Player1 (F.toList deck1)
      else do
        let previous' = Set.insert (deck1,deck2) previous
        case (Seq.viewl deck1 , Seq.viewl deck2) of
          (EmptyL  , _      ) -> do
             -- putStrLn "player #2 wins the game!"
             return $ Result Player2 (F.toList deck2)
          (_       , EmptyL ) -> do
             -- putStrLn "player #1 wins the game!"
             return $ Result Player1 (F.toList deck1)
          (x :< xs , y :< ys) -> if x <= Seq.length xs && y <= Seq.length ys
            then do
              subres <- subgame (Seq.take x xs) (Seq.take y ys)
              case subres of
                Result Player1 _ -> go previous' (xs |> x |> y) ys
                Result Player2 _ -> go previous' xs (ys |> y |> x)
            else if x > y 
              then go previous' (xs |> x |> y) ys 
              else go previous' xs (ys |> y |> x)

  subgame !deck1 !deck2 = do
    -- putStrLn "==================="
    -- putStrLn "SUBGAME!"    
    res <- go Set.empty deck1 deck2
    -- putStrLn "END OF SUBGAME"
    -- putStrLn "^^^^^^^^^^^^^^^^^^^"
    return res    

main = do
  decks <- load "input22"
  res@(Result winner final) <- play2 decks
  print res
  print $ sum $ zipWith (*) (reverse final) [1..]
