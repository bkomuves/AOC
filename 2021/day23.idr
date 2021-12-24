
import Data.Fin
import Data.Maybe
import Data.Vect
import Data.List
import Data.String

import Data.SortedSet
import Data.SortedMap

import Control.Monad.State

import Common
import Dijkstra

--------------------------------------------------------------------------------

data Ambi = A | B | C | D

Eq Ambi where
  (==) A A = True
  (==) B B = True
  (==) C C = True
  (==) D D = True
  (==) _ _ = False

Show Ambi where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"

ambiToInt : Ambi -> Int
ambiToInt A = 0
ambiToInt B = 1
ambiToInt C = 2
ambiToInt D = 3

Ord Ambi where
  compare a1 a2 = compare (ambiToInt a1) (ambiToInt a2)

showMbAmbi : Maybe Ambi -> String
showMbAmbi (Just a) = show a
showMbAmbi Nothing  = "."

Input : Nat -> Type
Input k = Vect k (Vect 4 Ambi)

--------------------------------------------------------------------------------
-- inputs for part 1

{-
#############
#...........#
###D#B#A#C###
  #C#A#D#B#
  #########
-}

input : Input 2
input = 
  [ [D,B,A,C]
  , [C,A,D,B]
  ]

example : Input 2
example = 
  [ [B,C,B,D]
  , [A,D,C,A]
  ]

finished : Input 2
finished = 
  [ [A,B,C,D]
  , [A,B,C,D]
  ]

--------------------------------------------------------------------------------
-- inputs for part 2

{-
Between the first and second lines of text that contain amphipod starting positions, insert the following lines:

  #D#C#B#A#
  #D#B#A#C#
-}

inputV2 : Input 4
inputV2 = 
  [ [D,B,A,C]
  , [D,C,B,A]
  , [D,B,A,C]
  , [C,A,D,B]
  ]

exampleV2 : Input 4
exampleV2 = 
  [ [B,C,B,D]
  , [D,C,B,A]
  , [D,B,A,C]
  , [A,D,C,A]
  ]

finishedV2 : Input 4
finishedV2 = 
  [ [A,B,C,D]
  , [A,B,C,D]
  , [A,B,C,D]
  , [A,B,C,D]
  ]

--------------------------------------------------------------------------------

data Pos : Nat -> Type where
  Hallway  : Fin 11          -> Pos k
  SideRoom : Fin 4  -> Fin k -> Pos k

mbHallway : Pos k -> Maybe (Fin 11)
mbHallway (Hallway x) = Just x
mbHallway _           = Nothing

mbSideRoom : Pos k -> Maybe (Fin 4, Fin k)
mbSideRoom (SideRoom x y) = Just (x,y)
mbSideRoom _              = Nothing

isHallway, isSideRoom : Pos k -> Bool
isHallway  = isJust . mbHallway
isSideRoom = isJust . mbSideRoom

listOfPositions : {k : Nat} -> List (Pos k)
listOfPositions  
  =  [ Hallway  x   | x <- toList (Fin.range {len=11}) ] 
  ++ concat  
       [ [ SideRoom j y | j <- toList (Fin.range {len=4 }) ]
       | y <- toList (Fin.range {len=k}) 
       ]

Eq (Pos k) where
  (==) (Hallway  x1   ) (Hallway  x2   ) = x1 == x2
  (==) (SideRoom x1 y1) (SideRoom x2 y2) = x1 == x2 && y1 == y2
  (==) _                _                = False

Ord (Pos k) where
  compare (Hallway  x1   ) (Hallway  x2   ) = compare x1 x2
  compare (Hallway  _    ) (SideRoom _  _ ) = LT
  compare (SideRoom _  _ ) (Hallway  _    ) = GT
  compare (SideRoom x1 y1) (SideRoom x2 y2) = case compare x1 x2 of
    LT => LT
    GT => GT
    EQ => compare y1 y2

Show (Pos k) where
  show (Hallway  x  ) = "Hallway x=" ++ show x
  show (SideRoom x y) = "SideRoom i=" ++ show x ++ " y=" ++ show y

--------------------------------------------------------------------------------

hallwaySide : Fin 11 -> Maybe (Fin 4)
hallwaySide 2 = Just 0
hallwaySide 4 = Just 1
hallwaySide 6 = Just 2
hallwaySide 8 = Just 3
hallwaySide _ = Nothing

sideHallway : Fin 4 -> Fin 11
sideHallway 0 = 2
sideHallway 1 = 4
sideHallway 2 = 6
sideHallway 3 = 8

neighbours : {k1 : Nat} -> Pos (S k1) -> List (Pos (S k1))
neighbours (Hallway k )   = catMaybes [ Hallway <$> decFin k , Hallway <$> incFin k , flip SideRoom 0 <$> hallwaySide k ]
neighbours (SideRoom x 0) = Hallway (sideHallway x) :: toList (SideRoom x <$> incFin 0)
neighbours (SideRoom x y) = catMaybes [ SideRoom x <$> incFin y , SideRoom x <$> decFin y ]

isValidHallwayPos : Fin 11 -> Bool
isValidHallwayPos = isNothing . hallwaySide

listValidHallways : {k : Nat} -> List (Pos k)
listValidHallways = [ Hallway x | x <- toList range, isValidHallwayPos x ]

--------------------------------------------------------------------------------

myTail : List a -> List a
myTail []      = []
myTail (_::xs) = xs

finRangeBidir : {n : Nat} -> Fin n -> Fin n -> List (Fin n)
finRangeBidir a b = if b >= a then finRange a b else reverse (finRange b a)

--------------------------------------------------------------------------------

-- excluding the starting point but including the endpoint
pathFromTo : {k : Nat} -> Pos k -> Pos k -> List (Pos k)

pathFromTo (Hallway x1) (Hallway x2) = myTail $ map Hallway $ finRangeBidir x1 x2

pathFromTo side1@(SideRoom x1 y1) side2@(SideRoom x2 y2) = if x1==x2
  then myTail [ SideRoom x1 y | y <- finRange y1 y2 ]
  else let up1 = Hallway (sideHallway x1)
           up2 = Hallway (sideHallway x2)
       in pathFromTo side1 up1 ++ pathFromTo up1 up2 ++ pathFromTo up2 side2

pathFromTo side1@(SideRoom x1 y1) hall2@(Hallway x2) = if sideHallway x1 == x2
  then myTail $ reverse [ SideRoom x1 y | y <- finRangeTo y1 ] ++ [ hall2 ] 
  else let up1 = Hallway (sideHallway x1)
       in pathFromTo side1 up1 ++ pathFromTo up1 hall2

pathFromTo hall1@(Hallway x1) side2@(SideRoom x2 y2) = if sideHallway x2 == x1
  then [ SideRoom x2 y | y <- finRangeTo y2 ]
  else let up2 = Hallway (sideHallway x2)
       in pathFromTo hall1 up2 ++ pathFromTo up2 side2

pathLength : {k : Nat} -> Pos k -> Pos k -> Nat
pathLength p1 p2 = length $ pathFromTo p1 p2

--------------------------------------------------------------------------------

Placement : Nat -> Type
Placement k = SortedMap (Pos k) Ambi

{-
Eq (Placement k) where 
  (==) pl1 pl2 = SortedMap.toList pl1 == SortedMap.toList pl2
-}

Ord (Placement k) where 
  compare pl1 pl2 = compare (SortedMap.toList pl1) (SortedMap.toList pl2)

showPlacement : {k : Nat} -> Placement k -> String
showPlacement placement = 
  let line0 =          concat [        showMbAmbi (lookup (Hallway  x  ) placement) | x <- Fin.range {len=11} ]
      lines = [ " " ++ concat [ " " ++ showMbAmbi (lookup (SideRoom j y) placement) | j <- Fin.range {len=4 } ]
              | y <- toList (Fin.range {len=k})
              ]
  in  unlines (line0::lines)

printPlacement : {k : Nat} -> Placement k -> IO ()
printPlacement = putStrLn . showPlacement

placementFromInput : {k : Nat} -> Input k -> Placement k
placementFromInput lines = SortedMap.fromList $ concatMap f (withIndices lines) where
  f : (Fin k, Vect 4 Ambi) -> List (Pos k, Ambi)
  f (y,as) = [ (SideRoom j y, a) | (j,a) <- toList (withIndices as) ]

--------------------------------------------------------------------------------

ambiRoom : Ambi -> Fin 4
ambiRoom A = 0
ambiRoom B = 1
ambiRoom C = 2
ambiRoom D = 3

roomOwner : Fin 4 -> Ambi
roomOwner 0 = A
roomOwner 1 = B
roomOwner 2 = C
roomOwner 3 = D

areWeFinished : {k : Nat} -> Placement k -> Bool
areWeFinished placement 
  =  and [ delay (lookup (Hallway  x  ) placement == Nothing)            | x <- toList (range {len=11}) ]
  && and [ delay (lookup (SideRoom j y) placement == Just (roomOwner j)) | j <- toList (range {len=4} ), y <- toList (range {len=k}) ]

roomIsReady : {k : Nat} -> Fin 4 -> Placement k -> Bool
roomIsReady j placement = and [ delay (ready y) | y <- range ] where
  ready : Fin k -> Bool
  ready i = case SortedMap.lookup (SideRoom j i) placement of
    Nothing => True
    Just a  => roomOwner j == a

canMoveThere : {k : Nat} -> Placement k -> Pos k -> Pos k -> Bool
canMoveThere placement start dest = start /= dest && case lookup start placement of
  Nothing   => False
  Just ambi => case lookup dest placement of
    Just _    => False
    Nothing   => and [ delay $ not (member q placement) | q <- pathFromTo start dest ]
              && case dest of 
                   { SideRoom j y => roomOwner j == ambi && roomIsReady j placement
                   ; Hallway  x   => not (isHallway start) && isValidHallwayPos x 
                   }        

--------------------------------------------------------------------------------

areThereForeignersInMyRoom : {k : Nat} -> Placement k -> Fin 4 -> Bool
areThereForeignersInMyRoom placement j = 
  let owner = roomOwner j
      f : Fin k -> Bool
      f = \y => case lookup (SideRoom j y) placement of { Nothing => False ; Just a  => a /= owner } 
  in  or [ delay (f y) | y <- toList range ]

canBeMoved : {k : Nat} -> Placement k -> Pos k -> Bool
canBeMoved placement pos = case lookup pos placement of
  Nothing   => False
  Just ambi => case pos of
    Hallway  x   => True
    SideRoom j i => if roomOwner j /= ambi
      then True
      else areThereForeignersInMyRoom placement j

--------------------------------------------------------------------------------

ambiMultiplier : Ambi -> Nat
ambiMultiplier A = 1
ambiMultiplier B = 10
ambiMultiplier C = 100
ambiMultiplier D = 1000

Energy : Type
Energy = Nat

executeMove : Placement k -> Pos k -> Pos k -> Placement k
executeMove placement from dest = case SortedMap.lookup from placement of
  Nothing   => fatal "executeMove: start position is empty"
  Just ambi => case SortedMap.lookup dest placement of
    Just _    => fatal "executeMove: destination is not empty"
    Nothing   => insert dest ambi $ delete from placement

validStartPositions : {k : Nat} -> Placement k -> List (Pos k, Ambi)
validStartPositions placement = catMaybes (map f listOfPositions) where
  f : Pos k -> Maybe (Pos k, Ambi)
  f p = case lookup p placement of 
    Nothing => Nothing 
    Just a  => if canBeMoved placement p
      then Just (p,a) 
      else Nothing

moves : {k : Nat} -> Placement k -> Maybe (List (Placement k, Energy))
moves placement = if areWeFinished placement
  then Nothing
  else 
    let validMoves = 
          [ (start,ambi,dest)
          | (start,ambi) <- validStartPositions placement
          , canBeMoved placement start
          , dest <- listOfPositions 
          , canMoveThere placement start dest
          ]
    in  Just
          [ (placement1,energy1)
          | (start,ambi,dest) <- validMoves
          , let path = pathFromTo start dest
          , let energy1 = ambiMultiplier ambi * length path
          , let placement1 = executeMove placement start dest
          ]

printMoves : {k : Nat} -> Placement k -> IO ()
printMoves what = case moves what of 
  Nothing   => putStrLn $ "finished"
  Just list => forM_ list $ \(pl,e) => do
    putStrLn $ "energy = " ++ show e
    printPlacement pl

--------------------------------------------------------------------------------

graphNeighbours : {k : Nat} -> Placement k -> List (Placement k, Energy)
graphNeighbours this = case moves this of
  Just list => list
  Nothing   => []

part1 : Input 2 -> IO ()
part1 input = do
  let placement   = placementFromInput input
  let destination = placementFromInput finished
  let answer = dijkstra_ graphNeighbours (placement,0) destination
  putStrLn $ "answer for part 1 = " ++ show answer

part2 : Input 4 -> IO ()
part2 input = do
  let placement   = placementFromInput input
  let destination = placementFromInput finishedV2
  let answer = dijkstra_ graphNeighbours (placement,0) destination
  putStrLn $ "answer for part 2 = " ++ show answer

--------------------------------------------------------------------------------

main : IO ()
main = do
  part1 input
  part2 inputV2

