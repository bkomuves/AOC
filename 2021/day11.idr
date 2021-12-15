
import Data.Maybe
import Data.Fin
import Data.Vect
import Data.List
import Data.SortedSet

import Common
import Matrix
import Digit

--------------------------------------------------------------------------------
-- part 1

increase : Matrix dim Digit -> Matrix dim (Maybe Digit)
increase = map @{matrix} incFin

flashToZero_ : Matrix dim (Maybe Digit) -> Matrix dim Digit
flashToZero_ = map @{matrix} f where
  f : Maybe Digit -> Digit
  f Nothing  = FZ
  f (Just d) = d

flashToZero : {dim : Dimensions} -> Matrix dim (Maybe Digit) -> (Nat, Matrix dim Digit)
flashToZero mat = (length (Matrix.findIndices isNothing mat) , flashToZero_ mat)

FlashSet : Dimensions -> Type
FlashSet dim = SortedSet (Index dim)

TodoList : Dimensions -> Type
TodoList dim = List (Index dim)

flash : {dim : Dimensions} -> Matrix dim (Maybe Digit) -> Matrix dim (Maybe Digit)
flash mat = go empty (findIndices isNothing mat) mat where

  incEnergy : Maybe Digit -> Maybe Digit
  incEnergy Nothing  = Nothing
  incEnergy (Just k) = incFin k

  updateAt1 : Index dim -> Matrix dim (Maybe Digit) -> (Maybe (Index dim), Matrix dim (Maybe Digit))
  updateAt1 idx mat = case index idx mat of
    Nothing => (Nothing, mat)
    Just k  => case incFin k of
      Nothing => (Just idx, replaceAt idx Nothing   mat)   -- flashed
      Just k1 => (Nothing , replaceAt idx (Just k1) mat)

  -- returns the new flashes
  updateAtMany : TodoList dim -> Matrix dim (Maybe Digit) -> (TodoList dim, Matrix dim (Maybe Digit))
  updateAtMany []      mat = ([],mat)
  updateAtMany (i::is) mat =
    let (mb,mat' ) = updateAt1    i  mat
        (ys,mat'') = updateAtMany is mat'
    in  case mb of
          Nothing => (ys   , mat'')
          Just y  => (y::ys, mat'')

  go : FlashSet dim -> TodoList dim -> Matrix dim (Maybe Digit) -> Matrix dim (Maybe Digit)
  go done Nil          mat = mat
  go done (this::rest) mat = if contains this done
    then go done rest mat
    else let done'  = insert this done
             neighs = allNeighbours this
             (new,mat') = updateAtMany neighs mat
             rest'  = rest ++ new
         in  go done' rest' mat'

step_ : {dim : Dimensions} -> Matrix dim Digit -> (Nat, Matrix dim Digit)
step_ = flashToZero . flash . increase

step : {dim : Dimensions} -> (Nat, Matrix dim Digit) -> (Nat, Matrix dim Digit)
step (acc,mat) = let (cnt,mat') = step_ mat in (acc+cnt,mat')

steps : {dim : Dimensions} -> Nat -> (Nat, Matrix dim Digit) -> (Nat, Matrix dim Digit)
steps n = natRec n step

debug : {dim : Dimensions} -> Nat -> Matrix dim Digit -> IO ()
debug Z     mat = pure ()
debug (S m) mat = do
  let (cnt,mat') = step_ mat
  putStrLn ""
  printDigitMatrix mat'
  putStrLn $ "number of flashes = " ++ show cnt
  debug m mat'

part1 : {dim : Dimensions} -> Matrix dim Digit -> IO ()
part1 octopus = do
  putStrLn "part 1"
  let n = the Nat 100
  let (cnt,_) = steps n (0,octopus)
  putStrLn $ "after " ++ show n ++ " steps, there were " ++ show cnt ++ " flashes"

--------------------------------------------------------------------------------
-- part 2

stepUntilSync : {dim : Dimensions} -> Nat -> Matrix dim Digit -> Nat
stepUntilSync n mat = let (cnt,mat') = step_ mat in
  if cnt == 100
    then (S n)
    else stepUntilSync (S n) mat'

part2 : {dim : Dimensions} -> Matrix dim Digit -> IO ()
part2 octopus = do
  putStrLn "part 2"
  let m = stepUntilSync 0 octopus
  putStrLn $ "after " ++ show m ++ " steps, all octopi flash simultaneously"

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input11"
  case parseDigitMatrix ls of
    Nothing   => fatal "not a rectangular shape"
    Just (dim ** octo) => do
      printDigitMatrix {dim=dim} octo
      part1 octo
      part2 octo
