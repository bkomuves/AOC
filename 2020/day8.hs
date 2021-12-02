
{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as Set

import Data.Array
import Control.Monad

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving Show

parseInstr :: String -> Instr
parseInstr str = case (take 3 str, drop 3 str) of
  (mnc,' ':sgn:num) -> let s = case sgn of { '+' -> 1 ; '-' -> -1 }
                           n = s * read num 
                       in  case mnc of
                             "acc" -> Acc n
                             "jmp" -> Jmp n
                             "nop" -> Nop n

readProgram :: FilePath -> IO [Instr]
readProgram fn = (map parseInstr . lines) <$> readFile fn

data Result
  = Loop        Int
  | Terminated  Int
  | OutOfBounds Int
  deriving Show

isTerminated (Terminated _) = True
isTerminated _              = False

run :: Array Int Instr -> Result
run prg = go 0 a Set.empty where
  (a,b) = bounds prg
  go !acc !pos !visited 
    | Set.member pos visited     = Loop        acc    -- infinite loop
    | pos == b+1                 = Terminated  acc    -- vegigfutott
    | pos < a || pos > b         = OutOfBounds acc
    | otherwise = case prg ! pos of
        Nop _ -> go  acc    (pos+1) visited'
        Acc k -> go (acc+k) (pos+1) visited'
        Jmp k -> go  acc    (pos+k) visited'
    where
      visited' = Set.insert pos visited

flipInstr prg i = case prg ! i of
  Jmp a -> Just (prg // [(i,Nop a)])
  Nop a -> Just (prg // [(i,Jmp a)])
  _     -> Nothing

main = do
  -- list <- readProgram "test8"
  list <- readProgram "input8"
  let n = length list
  let prg = listArray (1,n) list
  mapM_ print list
  print $ run prg
  let variations = [ prg' | i <- [1..n] , Just prg' <- [flipInstr prg i] ]
  let rs = map run variations
  print rs
  print $ filter isTerminated rs
