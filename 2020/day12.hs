
{-# LANGUAGE BangPatterns #-}

import Data.List

type Instr = (Char,Int)

parse :: String -> Instr
parse (c:rest) = (c,read rest)

data Dir = E | W | N | S deriving Show

dirv :: Dir -> Pos
dirv E = ( 1, 0)
dirv W = (-1, 0)
dirv N = ( 0, 1)
dirv S = ( 0,-1)

rotLeft1 E = N
rotLeft1 N = W
rotLeft1 W = S
rotLeft1 S = E

rotRight1 N = E 
rotRight1 W = N 
rotRight1 S = W 
rotRight1 E = S 

iter 0 f = id
iter n f = iter (n-1) f . f

rotLeft  n = iter n rotLeft1
rotRight n = iter n rotRight1

vrotLeft1 :: Pos -> Pos
vrotLeft1 (x,y) = (-y,x)

vrotRight1 :: Pos -> Pos
vrotRight1 (x,y) = (y,-x)

vrotLeft  n = iter n vrotLeft1
vrotRight n = iter n vrotRight1

vec :: Dir -> Int -> Pos
vec dir len = scale len (dirv dir)

scale :: Int -> Pos -> Pos
scale k (x,y) = (k*x,k*y)

type Pos = (Int,Int)

plus :: Pos -> Pos -> Pos
plus (a,b) (x,y) = (a+x , b+y)

type State = (Dir,Pos)

step :: State -> Instr -> State
step (!dir,!pos) (!c,!k) = case c of
  'E' -> (dir, plus pos (vec E k))
  'W' -> (dir, plus pos (vec W k))
  'N' -> (dir, plus pos (vec N k))
  'S' -> (dir, plus pos (vec S k))
  'L' -> case divMod k 90 of (n,0) -> (rotLeft  n dir , pos)
  'R' -> case divMod k 90 of (n,0) -> (rotRight n dir , pos)
  'F' -> (dir, plus pos (vec dir k))

type State2 = (Pos,Pos)

step_v2 :: State2 -> Instr -> State2
step_v2 (!wp,!pos) (!c,!k) = case c of
  'E' -> (plus wp (vec E k) , pos)
  'W' -> (plus wp (vec W k) , pos)
  'N' -> (plus wp (vec N k) , pos)
  'S' -> (plus wp (vec S k) , pos)
  'L' -> case divMod k 90 of (n,0) -> (vrotLeft  n wp , pos)
  'R' -> case divMod k 90 of (n,0) -> (vrotRight n wp , pos)
  'F' -> (wp, plus pos (scale k wp))

test = "F10 N3 F7 R90 F11"

main = do
  path <- (map parse . words) <$> readFile "input12"
  -- let path = map parse $ words $ test
  let ini  = (E,(0,0))
  let ini2 = ((10,1),(0,0))
  -- let end@(dir,pos) = foldl' step ini path
  let end@(dir,pos) = foldl' step_v2 ini2 path
  print end
  print (abs (fst pos) + abs (snd pos))
