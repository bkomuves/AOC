
import Data.Fin
import Data.Vect
import Data.List
import Data.List1
import Data.String
import Data.SortedSet

import Common
import Pos3D

--------------------------------------------------------------------------------

-- closed interval
Interval : Type
Interval = (Int,Int)

data Cuboid 
  = MkCuboid Interval Interval Interval

Eq Cuboid where
  (==) (MkCuboid xx1 yy1 zz1) (MkCuboid xx2 yy2 zz2) = (xx1 == xx2 && yy1 == yy2 && zz1 == zz2)

Show Cuboid where
  show (MkCuboid xx yy zz) = show xx ++ " # " ++ show yy ++ " # " ++ show zz

enumerateCuboid : Cuboid -> List Pos
enumerateCuboid (MkCuboid (x1,x2) (y1,y2) (z1,z2)) 
  = [ MkPos x y z | x<-[x1..x2], y<-[y1..y2], z<-[z1..z2] ]

cuboidVolume : Cuboid -> Integer
cuboidVolume (MkCuboid (x1,x2) (y1,y2) (z1,z2))
  = cast (x2-x1+1) 
  * cast (y2-y1+1) 
  * cast (z2-z1+1)

-- "bottom left" corner
cuboidCornerA : Cuboid -> Pos
cuboidCornerA (MkCuboid (x,_) (y,_) (z,_)) = MkPos x y z

-- "top right" corner
cuboidCornerB : Cuboid -> Pos
cuboidCornerB (MkCuboid (_,x) (_,y) (_,z)) = MkPos x y z

--------------------------------------------------------------------------------

data OnOff = On | Off

Show OnOff where 
  show On  = "on"
  show Off = "off"

record Step where
  constructor MkStep
  onOff  : OnOff
  cuboid : Cuboid

Show Step where
  show (MkStep off cub) = show off ++ " @@ " ++ show cub

--------------------------------------------------------------------------------
-- parsing

parseOnOff : String -> OnOff
parseOnOff "on"  = On
parseOnOff "off" = Off
parseOnOff _     = fatal "parseOnOff"

parseInterval : String -> Interval
parseInterval str = case split (=='.') str of
  (a:::""::b::Nil) => (cast a, cast b)
  _                => fatal "parseInterval"

parseNamedInterval : String -> (String,Interval)
parseNamedInterval str = case split (=='=') str of
  (name:::iv::Nil) => (name, parseInterval iv)  
  _                => fatal "parseNamedInterval"

parseCuboid : String -> Cuboid
parseCuboid str = case split (==',') str of
  (xx:::yy::zz::Nil) => 
    let (_, x1x2) = parseNamedInterval xx
        (_, y1y2) = parseNamedInterval yy
        (_, z1z2) = parseNamedInterval zz
    in  MkCuboid x1x2 y1y2 z1z2
  _ => fatal "parseCuboid"

parseStep : String -> Step
parseStep str = case split (==' ') str of
  (off:::[cub]) => MkStep (parseOnOff off) (parseCuboid cub)
  _             => fatal "parseStep"

--------------------------------------------------------------------------------
-- part 1

isSmallInterval : Interval -> Bool
isSmallInterval (u,v) = u >= -50 && v <= 50

isSmallCuboid : Cuboid -> Bool
isSmallCuboid (MkCuboid xx yy zz) 
  =  isSmallInterval xx
  && isSmallInterval yy
  && isSmallInterval zz

isSmallStep : Step -> Bool
isSmallStep step = isSmallCuboid step.cuboid

step : Step -> SortedSet Pos -> SortedSet Pos 
step (MkStep onoff cuboid) old = case onoff of
  Off => foldl (\set, p => delete p set) old (enumerateCuboid cuboid)
  On  => foldl (\set, p => insert p set) old (enumerateCuboid cuboid)

part1 : List Step -> IO ()
part1 allSteps = do
  let state = foldl (flip step) empty (filter isSmallStep allSteps)
  let n = length (SortedSet.toList state)
  putStrLn $ "answer to part 1 = " ++ show n

--------------------------------------------------------------------------------
-- coordinate axes

data Axis 
  = XAxis 
  | YAxis 
  | ZAxis

Show Axis where
  show XAxis = "X"
  show YAxis = "Y"
  show ZAxis = "Z"

project : Axis -> Cuboid -> Interval
project axis (MkCuboid xx yy zz) = case axis of
  XAxis => xx
  YAxis => yy
  ZAxis => zz

--------------------------------------------------------------------------------
-- coordinate hyperplanes

-- a hyperplane orthogonal to an axis
record Plane where
  constructor MkPlane
  axis  : Axis
  coord : Int

Show Plane where
  show (MkPlane axis coord) = show axis ++ "=" ++ show coord

cuboidPlanesVec : Cuboid -> Vect 6 Plane
cuboidPlanesVec (MkCuboid (x1,x2) (y1,y2) (z1,z2)) = 
  [ MkPlane XAxis  x1
  , MkPlane XAxis (x2+1)
  , MkPlane YAxis  y1
  , MkPlane YAxis (y2+1)
  , MkPlane ZAxis  z1
  , MkPlane ZAxis (z2+1)
  ]

cuboidPlanes : Cuboid -> List Plane
cuboidPlanes = toList . cuboidPlanesVec

--------------------------------------------------------------------------------
-- cutting along planes

namespace Cut

  -- result of cutting something along a straight line or plane
  public export
  data CutResult a
    = Below  a
    | Pieces a a
    | Above    a

  public export
  Functor CutResult where
    map f (Below  x  ) = Below  (f x)
    map f (Pieces x y) = Pieces (f x) (f y)
    map f (Above    y) = Above        (f y)

  cutResultToList : CutResult a -> List a
  cutResultToList (Below  x  ) = [x]
  cutResultToList (Pieces x y) = [x,y]
  cutResultToList (Above    y) = [y]

  export
  cutInterval : Int -> Interval -> CutResult Interval
  cutInterval cut ab@(a,b) = if cut <= a 
    then Above ab
    else if cut > b 
      then Below ab
      else Pieces (a,cut-1) (cut,b) 

  mapOnAxis : Axis -> (Interval -> Interval) -> (Cuboid -> Cuboid)
  mapOnAxis XAxis f (MkCuboid xx yy zz) = MkCuboid (f xx)    yy     zz
  mapOnAxis YAxis f (MkCuboid xx yy zz) = MkCuboid    xx  (f yy)    zz
  mapOnAxis ZAxis f (MkCuboid xx yy zz) = MkCuboid    xx     yy  (f zz)

  replaceAxis : Axis -> Interval -> (Cuboid -> Cuboid)
  replaceAxis axis iv = mapOnAxis axis (const iv)

  export
  cutCuboid : Plane -> Cuboid -> CutResult Cuboid
  cutCuboid (MkPlane axis cut) cuboid@(MkCuboid xx yy zz) = case axis of
    XAxis => map (\iv => replaceAxis XAxis iv cuboid) (cutInterval cut xx) 
    YAxis => map (\iv => replaceAxis YAxis iv cuboid) (cutInterval cut yy) 
    ZAxis => map (\iv => replaceAxis ZAxis iv cuboid) (cutInterval cut zz) 

  export
  cutCuboid_ : Plane -> Cuboid -> List Cuboid
  cutCuboid_ p c = cutResultToList $ cutCuboid p c

--------------------------------------------------------------------------------
-- intersection, difference and union of cuboids

isSubIntervalOf : Interval -> Interval -> Bool
isSubIntervalOf (a,b) (c,d) = a >= c && b <= d

isSubCuboidOf : Cuboid -> Cuboid -> Bool
isSubCuboidOf (MkCuboid xx1 yy1 zz1) (MkCuboid xx2 yy2 zz2) 
  =  isSubIntervalOf xx1 xx2
  && isSubIntervalOf yy1 yy2
  && isSubIntervalOf zz1 zz2

intersectInterval : Interval -> Interval -> Maybe Interval
intersectInterval (a,b) (c,d) = 
  let u = max a c
      v = min b d
  in  if v >= u then Just (u,v) else Nothing

intersection : Cuboid -> Cuboid -> Maybe Cuboid
intersection (MkCuboid xx1 yy1 zz1) (MkCuboid xx2 yy2 zz2) = do
  uu <- intersectInterval xx1 xx2
  vv <- intersectInterval yy1 yy2
  ww <- intersectInterval zz1 zz2
  pure (MkCuboid uu vv ww)

cutAlong : List Plane -> List Cuboid -> List Cuboid
cutAlong []      cs = cs
cutAlong (p::ps) cs = cutAlong ps $ concatMap (cutCuboid_ p) cs

difference : Cuboid -> Cuboid -> List Cuboid
difference cub1 cub2 = case intersection cub1 cub2 of 
  Nothing => [cub1]
  Just is => filter (\d => not (isSubCuboidOf d cub2)) 
           $ cutAlong (cuboidPlanes is) [cub1] 

union : Cuboid -> Cuboid -> List Cuboid
union cub1 cub2 = cub1 :: difference cub2 cub1

{-
ex1, ex2 : Cuboid
ex1 = MkCuboid (10,20) (20,40) (0,0)
ex2 = MkCuboid (15,25) (28,48) (0,0)
ex3 = MkCuboid (30,40) (20,40) (0,0)
-}

--------------------------------------------------------------------------------
-- part 2

listVolume : List Cuboid -> Integer
listVolume = sum . map cuboidVolume

stepV2 : Step -> List Cuboid -> List Cuboid
stepV2 (MkStep onoff this) old = case onoff of
  Off =>         concatMap (\cub => difference cub this) old
  On  => this :: concatMap (\cub => difference cub this) old

part2 : List Step -> IO ()
part2 allSteps = do
  putStrLn "\npart 2 (be patient, this takes about 30 seconds on my machine)"
  let steps = allSteps -- filter isSmallStep allSteps
  let final = foldl (flip stepV2) [] steps
  let n = listVolume final
  putStrLn $ "answer to part 2 = " ++ show n

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input22" 
  let steps = map parseStep ls
  -- mapM_ printLn steps
  part1 steps
  part2 steps
