
{-# LANGUAGE Strict, BangPatterns #-}

import Data.List
import Data.List.Split
import Data.Bits

import qualified Data.Map as Map
import Data.Map (Map)

--------------------------------------------------------------------------------

example1 = unlines 
  [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  , "mem[8] = 11"
  , "mem[7] = 101"
  , "mem[8] = 0"
  ]

example2 = unlines
  [ "mask = 000000000000000000000000000000X1001X"
  , "mem[42] = 100"
  , "mask = 00000000000000000000000000000000X0XX"
  , "mem[26] = 1"
  ]

--------------------------------------------------------------------------------

type Program = [Cmd]

data Cmd 
  = MaskCmd Mask
  | MemCmd  (Int,Int)
  deriving Show

data Mask = Mask
  { _maskBits  :: !Int       -- 0-s and 1-s
  , _valueBits :: !Int
  , _maskBits2 :: !Int       -- only 1-s
  , _floating  :: ![Int]
  }
  deriving Show

--------------------------------------------------------------------------------
-- parsing

parseLine :: String -> Cmd
parseLine str 
  | isPrefixOf "mask = " str = MaskCmd $ parseMask (drop 7 str)
  | otherwise                = MemCmd  $ parseMem str

parseMask :: String -> Mask
parseMask str = (Mask mask value mask2 floating) where
  bits  = parseMask_ str
  mask  = toMaskBits     bits
  mask2 = toMaskBits_v2  bits
  value = toValueBits bits
  floating = toFloating bits

parseMask_ :: String -> String
parseMask_ str = if length str == 36 then str else error "header"

parseMem :: String -> (Int,Int)
parseMem str = case splitOn "] = " str of
  [left,right] -> if isPrefixOf "mem[" left then (read (drop 4 left), read right) else error "cmd"

parseProgram :: String -> Program
parseProgram = map parseLine . lines 

--------------------------------------------------------------------------------
-- binary bits

decodeBinary :: [Bool] -> Int
decodeBinary = go 0 where
  go acc [] = acc
  go acc (this:rest) = go acc' rest where acc' = (if this then 1 else 0) + shiftL acc 1

toMaskBits :: String -> Int
toMaskBits = decodeBinary . map f where
  f 'X' = True
  f _   = False

toMaskBits_v2 :: String -> Int
toMaskBits_v2 = decodeBinary . map h where
  h '1' = True
  h _   = False

toValueBits :: String -> Int
toValueBits = decodeBinary . map g where
  g '1' = True
  g _   = False

toFloating :: String -> [Int]
toFloating string = [ i | (i,c) <- zip [0..] (reverse string) , c == 'X' ]

--------------------------------------------------------------------------------
-- executing the VM

applyMask :: Mask -> Int -> Int
applyMask (Mask mask value _ _) x = (x .&. mask) .|. value

applyMaskFloating :: Mask -> Int -> [Int]
applyMaskFloating (Mask _ value mask floating) x0 = worker x1 floating where
  x1 = (x0 .&. complement mask) .|. value

  worker !x []     = [x]
  worker !x (j:js) = worker (clearBit x j) js ++ worker (setBit x j) js

type Memory = Map Int Int

exec_v1 :: Program -> (Mask,Memory)
exec_v1 cmds = foldl' step ini cmds where
  ini = (Mask 0 0 0 [] , Map.empty)
  step (!mask,!memory) cmd = case cmd of
    MaskCmd !newmask      -> (newmask,memory)
    MemCmd  (!loc,!value) -> (mask,memory') where
      memory' = Map.insert loc value' memory 
      value'  = applyMask mask value

exec_v2 :: Program -> (Mask,Memory)
exec_v2 cmds = foldl' step ini cmds where
  ini = (Mask 0 0 0 [] , Map.empty)
  step (!mask,!memory) cmd = case cmd of
    MaskCmd !newmask      -> (newmask,memory)
    MemCmd  (!loc,!value) -> (mask,memory') where
      memory' = foldl' (\mem loc -> Map.insert loc value mem) memory locs
      locs    = applyMaskFloating mask loc :: [Int]

--------------------------------------------------------------------------------

testmain = do
  let mask1 = parseMask "000000000000000000000000000000X1001X"
  let mask2 = parseMask "00000000000000000000000000000000X0XX"
  print mask1
  print $ applyMaskFloating mask1 42
  print mask2
  print $ applyMaskFloating mask2 26

solve1 prg = do
  putStrLn "\npart 1"
  let (_,mem) = exec_v1 prg
  -- print mem
  print $ sum $ Map.elems mem

solve2 prg = do
  putStrLn "\npart 2"
  let (_,mem) = exec_v2 prg
  -- print mem
  print $ sum $ Map.elems mem

main = do
  -- let text = example1
  -- let text = example2
  text <- readFile "input14"
  let prg = parseProgram text
  solve1 prg
  solve2 prg
