
import Data.List
import Data.String
import System.File

import Common

--------------------------------------------------------------------------------

data Dir
  = Fwd 
  | Up  
  | Down

data Move 
  = MkMove Dir Int

parseDir : String -> Dir
parseDir "forward" = Fwd
parseDir "up"      = Up
parseDir "down"    = Down
parseDir _         = fatal "cannot parse direction"

parseLine : String -> Move
parseLine str = case words str of
  [dir,n] => MkMove (parseDir dir) (readInt n)
  _       => fatal "cannot parse line"

--------------------------------------------------------------------------------
-- part 1

data Pos 
  = MkPos Int Int

zeroPos : Pos
zeroPos = MkPos 0 0

mulPos : Pos -> Int
mulPos (MkPos x y) = x*y

move : Move -> Pos -> Pos
move (MkMove dir a) (MkPos x y) = case dir of
  Fwd  => MkPos (x+a) y
  Up   => MkPos x (y-a)
  Down => MkPos x (y+a)

solve1 : List Move -> Int
solve1 = mulPos . foldl (flip move) zeroPos

--------------------------------------------------------------------------------
-- part 2

Aim : Type
Aim = Int

record StateV2 where
  constructor MkV2 
  pos : Pos 
  aim : Aim

zeroV2 : StateV2
zeroV2 = MkV2 zeroPos 0

updateV2 : Move -> StateV2 -> StateV2
updateV2 move@(MkMove dir a) (MkV2 pos@(MkPos x y) aim) = case dir of
  Up   => MkV2 pos (aim - a)
  Down => MkV2 pos (aim + a)
  Fwd  => MkV2 (MkPos (x + a) (y + aim*a)) aim

mulV2 : StateV2 -> Int
mulV2 s = mulPos s.pos

solve2 : List Move -> Int
solve2 = mulV2 . foldl (flip updateV2) zeroV2

--------------------------------------------------------------------------------

main : IO ()
main = do
  lines <- readLines "input2"
  let input = map parseLine $ filter (not . null) lines
  printLn $ solve1 input
  printLn $ solve2 input

