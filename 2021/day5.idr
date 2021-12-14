
import Data.List
import Data.List1
import Data.String
import Data.SortedMap

import Common
import Pos

--------------------------------------------------------------------------------
-- parsing

Line : Type
Line = (Pos,Pos)

parseLine : String -> Line
parseLine str = case split (=='-') str of
  (start:::[end1]) => (parsePos start, parsePos (safeTail_ end1))
  _                => fatal $ "cannot parse line: " ++ show str 

Input : Type
Input = List Line

--------------------------------------------------------------------------------
-- part 1

Dist : Type
Dist = Nat

namespace Part1

  data HV
    = Horz Pos Dist
    | Vert Pos Dist

  Show HV where
    show (Horz pos dist) = "horizontal " ++ show pos ++ " => " ++ show dist
    show (Vert pos dist) = "vertical "   ++ show pos ++ " => " ++ show dist

  toHV : (Pos,Pos) -> Maybe HV
  toHV (MkPos x1 y1, MkPos x2 y2) =
    if y1 == y2
      then Just $ Horz (MkPos (min x1 x2) y1) (cast $ abs (x2-x1))
      else if x1 == x2
        then Just $ Vert (MkPos x1 (min y1 y2)) (cast $ abs (y2-y1))
        else Nothing

  hvToPixels : HV -> List Pos
  hvToPixels (Horz (MkPos x y) d) = [ MkPos (x + cast i) y | i<-[0..d] ]
  hvToPixels (Vert (MkPos x y) d) = [ MkPos x (y + cast i) | i<-[0..d] ]

  hvToPixelSet : HV -> SortedMap Pos Nat
  hvToPixelSet = fromList . map (\p => (p,1)) . hvToPixels

  export
  solve1 : Input -> IO ()
  solve1 input = do
    let hvs   = mapMaybe toHV input
    let msets = map hvToPixelSet hvs
    let final : SortedMap Pos Nat
        final = foldl (mergeWith (+)) empty msets
    let atLeastTwo : List Pos 
        atLeastTwo = [ xy | (xy,cnt) <- SortedMap.toList final , cnt >= 2 ]
    -- printLn atLeastTwo
    putStrLn $ "part1 = " ++ show (length atLeastTwo)

--------------------------------------------------------------------------------
-- part2 

namespace Part2

  data HVD 
    = Horz   Pos Dist
    | Vert   Pos Dist
    | DiagDn Pos Dist
    | DiagUp Pos Dist
  
  Show HVD where
    show (Horz   pos dist) = "horizontal "      ++ show pos ++ " => " ++ show dist
    show (Vert   pos dist) = "vertical "        ++ show pos ++ " => " ++ show dist
    show (DiagDn pos dist) = "diagonal (down) " ++ show pos ++ " => " ++ show dist
    show (DiagUp pos dist) = "diagonal (up) "   ++ show pos ++ " => " ++ show dist
  
  toHVD : (Pos,Pos) -> Maybe HVD
  toHVD (MkPos x1 y1, MkPos x2 y2) =
    if y1 == y2
      then Just $ Horz (MkPos (min x1 x2) y1) (cast $ abs (x2-x1))
      else if x1 == x2
        then Just $ Vert (MkPos x1 (min y1 y2)) (cast $ abs (y2-y1))
        else if x2 - x1 == y2 - y1 
          then Just $ DiagDn (MkPos (min x1 x2) (min y1 y2)) (cast $ abs (x2-x1))
          else if x2 - x1 == y1 - y2
            then Just $ DiagUp (MkPos (min x1 x2) (max y1 y2)) (cast $ abs (x2-x1))
            else Nothing
  
  unsafeToHVD : (Pos,Pos) -> HVD
  unsafeToHVD ln = case toHVD ln of
    Just hvd => hvd
    Nothing  => fatal "unsafeToHVD: not horizontal, vertical or diagonal"
  
  hvdToPixels : HVD -> List Pos
  hvdToPixels (Horz   (MkPos x y) d) = [ MkPos (x + cast i)  y           | i<-[0..d] ]
  hvdToPixels (Vert   (MkPos x y) d) = [ MkPos  x           (y + cast i) | i<-[0..d] ]
  hvdToPixels (DiagDn (MkPos x y) d) = [ MkPos (x + cast i) (y + cast i) | i<-[0..d] ]
  hvdToPixels (DiagUp (MkPos x y) d) = [ MkPos (x + cast i) (y - cast i) | i<-[0..d] ]
  
  hvdToPixelSet : HVD -> SortedMap Pos Nat
  hvdToPixelSet = fromList . map (\p => (p,1)) . hvdToPixels
  
  export
  solve2 : Input -> IO ()
  solve2 input = do
    let hvds  = map unsafeToHVD input
    let msets = map hvdToPixelSet hvds
    let final : SortedMap Pos Nat
        final = foldl (mergeWith (+)) empty msets
    let atLeastTwo : List Pos 
        atLeastTwo = [ xy | (xy,cnt) <- SortedMap.toList final , cnt >= 2 ]
    putStrLn $ "part2 = " ++ show (length atLeastTwo)

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input5"
  let input : List Line
      input = map parseLine $ filterNonEmpty ls
  -- mapM_ printLn input
  solve1 input
  solve2 input

