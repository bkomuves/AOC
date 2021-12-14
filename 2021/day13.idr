
import Data.List
import Data.List1
import Data.String
import Data.SortedSet

import Common
import Pos

--------------------------------------------------------------------------------
-- types

data Fold
  = FoldAlongX Int
  | FoldAlongY Int

Show Fold where
  show (FoldAlongX x) = "FoldAlong x=" ++ show x
  show (FoldAlongY y) = "FoldAlong y=" ++ show y

record Input where
  constructor MkInput
  dots  : List Pos
  folds : List Fold

applyFold1 : Fold -> Pos -> Pos
applyFold1 (FoldAlongX s) (MkPos x y) = MkPos   (if x < s then x else 2*s-x) y
applyFold1 (FoldAlongY t) (MkPos x y) = MkPos x (if y < t then y else 2*t-y)

applyFold : Fold -> SortedSet Pos -> SortedSet Pos
applyFold fold = SortedSet.fromList . map (applyFold1 fold) . SortedSet.toList

applyFolds : List Fold -> SortedSet Pos -> SortedSet Pos
applyFolds folds set = foldl (flip applyFold) set folds

--------------------------------------------------------------------------------
-- parsing

parseFold : String -> Fold
parseFold str = case split (==' ') str of
  ("fold":::["along",what]) => case split (=='=') what of
    (xy:::[coord]) => case xy of
      "x" => FoldAlongX (cast coord)
      "y" => FoldAlongY (cast coord)
      _   => cannotParse
    _ => cannotParse
  _ => cannotParse
  where
    cannotParse : Fold
    cannotParse = fatal $ "canot parse fold instruction `" ++ str ++ "`"

parseInput : List String -> Input
parseInput ls = case split null ls of
    (top:::(bottom::_)) => MkInput (map parsePos top) (map parseFold bottom)
    _ => fatal "cannot parse input"

--------------------------------------------------------------------------------
-- part 1

part1 : Input -> IO ()
part1 (MkInput dots folds) = do
  putStrLn "part 1"
  case folds of
    (first::_) => do
      let folded = applyFold first (SortedSet.fromList dots)
      putStrLn $ "the answer = " ++ show (length $ SortedSet.toList folded)
    _ => fatal "empty list of fold instructions"

--------------------------------------------------------------------------------
-- part 2

bottomRightCorner : List Pos -> Pos
bottomRightCorner list = MkPos
  (foldl max 0 (map xcoord list))
  (foldl max 0 (map ycoord list))

part2 : Input -> IO ()
part2 (MkInput dots folds) = do
  putStrLn "\npart 2\n============"
  let folded = applyFolds folds (SortedSet.fromList dots)
  let MkPos m n = bottomRightCorner (SortedSet.toList folded)
  forM_ [0..n] $ \y => do
    let cs = [ (if contains (MkPos x y) folded then '#' else '.') | x<-[0..m] ]
    putStrLn (pack cs)

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input13"
  let input = parseInput ls
  -- mapM_ printLn (input.dots)
  -- mapM_ printLn (input.folds)
  part1 input
  part2 input

--------------------------------------------------------------------------------
