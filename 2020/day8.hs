
{-# LANGUAGE Strict #-}

import qualified Data.Set as Set

import Data.Array
import Control.Monad

--------------------------------------------------------------------------------
-- parsing

data Instr
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving Show

parseInstr :: String -> Instr
parseInstr str = case (take 3 str, drop 3 str) of
  (mnc,' ':sgn:num) -> 
    let s = case sgn of { '+' -> 1 ; '-' -> -1 }
        n = s * read num 
    in  case mnc of
          "acc" -> Acc n
          "jmp" -> Jmp n
          "nop" -> Nop n

readProgram :: FilePath -> IO [Instr]
readProgram fn = (map parseInstr . lines) <$> readFile fn

type Prg = Array Int Instr

--------------------------------------------------------------------------------

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
  go acc pos visited 
    | Set.member pos visited     = Loop        acc    -- infinite loop
    | pos == b+1                 = Terminated  acc    -- finished
    | pos < a || pos > b         = OutOfBounds acc    -- error
    | otherwise = case prg ! pos of
        Nop _ -> go  acc    (pos+1) visited'
        Acc k -> go (acc+k) (pos+1) visited'
        Jmp k -> go  acc    (pos+k) visited'
    where
      visited' = Set.insert pos visited

flipInstr :: Array Int Instr -> Int -> Maybe (Array Int Instr)
flipInstr prg i = case prg ! i of
  Jmp a -> Just (prg // [(i,Nop a)])
  Nop a -> Just (prg // [(i,Jmp a)])
  _     -> Nothing

--------------------------------------------------------------------------------

part1 :: Prg -> IO ()
part1 prg = do
  putStrLn $ "part 1: " ++ show (run prg)

part2 :: Prg -> IO ()
part2 prg = do
  let (1,n) = bounds prg
  let variations = [ prg' | i <- [1..n] , Just prg' <- [flipInstr prg i] ]
  let rs = map run variations
  -- print rs
  putStrLn $ "part2 : " ++ show (filter isTerminated rs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  list <- readProgram "input8"
  -- mapM_ print list
  let prg = listArray (1,length list) list
  part1 prg
  part2 prg

