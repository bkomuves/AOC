
import Data.Fin
import Data.Vect
import Data.String

import Common
import Matrix

--------------------------------------------------------------------------------

data Pixel 
  = Dark | Light

showPixel : Pixel -> String
showPixel Dark  = "."
showPixel Light = "#"

parsePixel : Char -> Maybe Pixel
parsePixel '.' = Just Dark
parsePixel '#' = Just Light
parsePixel _   = Nothing

--------------------------------------------------------------------------------

LkpTable : Type
LkpTable = Vect 512 Pixel

record Input where
  constructor MkInput
  lkpTable   : LkpTable
  background : Pixel
  dim        : Dimensions
  image      : Matrix dim Pixel

printInput : Input -> IO ()
printInput (MkInput _ _ _ image) = printMatrix showPixel image

parseHeader : String -> LkpTable
parseHeader str = case allJust (map parsePixel $ unpack $ trim str) of 
  Nothing => fatal "cannot parse lookup table"
  Just ps => case fromListN 512 ps of
    Nothing => fatal "lookup table length /= 512"
    Just v  => v

--------------------------------------------------------------------------------
-- nine pixels

namespace Nat

  public export
  Plus2 : Nat -> Nat
  Plus2 n = S (S n)

  public export
  twice : Nat -> Nat
  twice Z     = Z
  twice (S k) = Plus2 (twice k)

  public export
  power2 : Nat -> Nat
  power2 Z     = 1
  power2 (S n) = twice (power2 n)

namespace Fin

  public export
  twice : Fin n -> Fin (Nat.twice n)
  twice FZ     = FZ 
  twice (FS k) = FS (FS (twice k))

  public export
  twicePlus1 : Fin n -> Fin (Nat.twice n)
  twicePlus1 FZ     = FS FZ 
  twicePlus1 (FS k) = FS (FS (twicePlus1 k))

npixelsLE : Vect n Pixel -> Fin (power2 n)
npixelsLE Nil         = FZ
npixelsLE (Dark ::ps) = twice      (npixelsLE ps)
npixelsLE (Light::ps) = twicePlus1 (npixelsLE ps)

npixelsBE : Vect n Pixel -> Fin (power2 n)
npixelsBE = npixelsLE . reverse

ninepixels : Vect 9 Pixel -> Fin 512
ninepixels = npixelsBE

--------------------------------------------------------------------------------
-- enhance

abc : List Int
abc = [-1,0,1]

ijs : Vect 9 (Int,Int)
ijs = fromList [ (i,j) | i<-abc , j<-abc ]

finToInt : Fin n -> Int
finToInt = cast . finToNat

intToFin : Int -> (n : Nat) -> Maybe (Fin n)
intToFin k n = if k < 0 then Nothing else natToFin (cast k) n

address : a -> {dim : Dimensions} -> Matrix dim a -> (Int,Int) -> a
address def {dim=(n,m)} mtx (i,j) = case (intToFin i n, intToFin j m) of
  (Just fi, Just fj) => index (MkIndex fi fj) mtx
  _                  => def

window2 : {n, m : Nat} -> Pixel -> Matrix (n,m) Pixel -> Index (Plus2 n, Plus2 m) -> Vect 9 Pixel
window2 bgnd mtx (MkIndex i j) = map f ijs where
  f : (Int,Int) -> Pixel
  f (p,q) = address bgnd {dim=(n,m)} mtx (finToInt i - 1 + p, finToInt j - 1 + q)

dec : {k : Nat} -> Fin (Plus2 k) -> Maybe (Fin k)
dec i = intToFin (finToInt i - 1) k

{-
-- this is actually unused
extendMatrix : {n,m : Nat} -> a -> Matrix (n,m) a -> Matrix (Plus2 n , Plus2 m) a
extendMatrix def old = mkMatrix f where  
  f : Index (Plus2 n, Plus2 m) -> a
  f (MkIndex i j) = case (dec i, dec j) of
    (Just fi, Just fj) => index (MkIndex fi fj) old
    _                  => def
-}

step : Input -> Input
step (MkInput table oldbgnd (n,m) old) = MkInput table newbgnd (N2,M2) new where

  N2,M2 : Nat
  N2 = Plus2 n
  M2 = Plus2 m

  transform : Vect 9 Pixel -> Pixel
  transform nine = index (ninepixels nine) table

  mk : Index (N2,M2) -> Pixel
  mk ij = transform (window2 oldbgnd old ij)

  new : Matrix (N2,M2) Pixel
  new = mkMatrix mk 

  newbgnd : Pixel
  newbgnd = case oldbgnd of
    Dark  => index FZ   table 
    Light => index last table

--------------------------------------------------------------------------------
-- part 1

namespace Pixel
  public export
  countPixels : Pixel -> Int
  countPixels Light = 1
  countPixels Dark  = 0

namespace Vect
  public export
  countPixels : Vect n Pixel -> Int
  countPixels = sum . map Pixel.countPixels

namespace Matrix
  public export
  countPixels : {dim : Dimensions} -> Matrix dim Pixel -> Int
  countPixels {dim=(_,_)} = sum . map Vect.countPixels

--------------------------------------------------------------------------------

iterateN : Nat -> Input -> IO ()
iterateN n input0 = do
  let inputN = natRec n step input0
  let cnt = Matrix.countPixels inputN.image
  let cnt_str = case inputN.background of
        Light => "infinity"
        Dark  => show cnt 
  putStrLn $ "number of light pixels after " ++ show n ++ " steps = " ++ cnt_str

part1, part2 : Input -> IO ()
part1 input = iterateN  2 input
part2 input = iterateN 50 input

--------------------------------------------------------------------------------

parts12 : Input -> IO ()
parts12 input = do
  -- printInput input
  -- putStrLn "--------"
  -- printInput $ step input
  -- putStrLn "--------"
  -- printInput $ step $ step input
  -- putStrLn "--------"
  part1 input
  part2 input

main : IO ()
main = do
  ls <- readLines "input20"
  case ls of 
    (header::""::rest) => do
      let table = parseHeader header
      case parseMatrix parsePixel $ filterNonEmpty rest of
        Nothing           => fatal "cannot parse input file"
        Just (dim ** mtx) => do 
          let input = MkInput table Dark dim mtx
          parts12 input
    _ => fatal "invalid input file structure"

