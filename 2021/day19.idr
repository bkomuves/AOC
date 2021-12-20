
import Data.List
import Data.List1
import Data.String
import Data.Vect

import Data.SortedSet
import Data.SortedMap

import Common

--------------------------------------------------------------------------------
-- absolute positions

namespace Pos

  public export
  data Pos 
    = MkPos Int Int Int
  
  public export
  Eq Pos where
    (==) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = (x1==x2) && (y1==y2) && (z1==z2)
  
  public export
  Ord Pos where
    compare (MkPos x1 y1 z1) (MkPos x2 y2 z2) = case compare x1 x2 of
      LT => LT
      GT => GT
      EQ => case compare y1 y2 of
        LT => LT
        GT => GT
        EQ => compare z1 z2
  
  public export
  Show Pos where
    show (MkPos x y z) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " )"

  infixl 6 +
  public export
  (+) : Pos -> Pos -> Pos
  (+) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = MkPos (x1+x2) (y1+y2) (z1+z2)
  
  infixl 6 -
  public export
  (-) : Pos -> Pos -> Pos
  (-) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = MkPos (x1-x2) (y1-y2) (z1-z2)

  public export
  negate : Pos -> Pos
  negate (MkPos x y z) = MkPos (-x) (-y) (-z)

  public export
  absSquare : Pos -> Int
  absSquare (MkPos x y z) = x*x + y*y + z*z

  public export
  distSquare : Pos -> Pos -> Int
  distSquare (MkPos x1 y1 z1) (MkPos x2 y2 z2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)

  public export
  manhDist : Pos -> Pos -> Int
  manhDist (MkPos x1 y1 z1) (MkPos x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

  public export
  parsePos : String -> Maybe Pos
  parsePos str = case split (==',') (trim str) of 
    x:::y::z::[] => Just $ MkPos (cast x) (cast y) (cast z)
    _            => Nothing

  public export
  parsePos_ : String -> Pos
  parsePos_ str = case parsePos str of
    Just pos => pos
    Nothing  => fatal "cannot parse position"

--------------------------------------------------------------------------------
-- parsing

Scan : Type
Scan = List Pos

parseScanner : List String -> Scan
parseScanner Nil = fatal "parseScanner: empty input"
parseScanner (header::rest) = if isPrefixOf "---" header
  then map parsePos_ rest
  else fatal "parseScanner: non-scanner input"

--------------------------------------------------------------------------------
-- flips and permutations

data Flip 
  = MkFlip Bool Bool Bool

Show Flip where
  show (MkFlip a b c) = pack 
    [ '<' 
    , (if a then '-' else '+')
    , (if b then '-' else '+')
    , (if c then '-' else '+')
    , '>'
    ]

tf : List Bool
tf = [False,True]

allFlips : List Flip
allFlips = [ MkFlip a b c | a<-tf, b<-tf, c<-tf ]

applyFlip1 : Flip -> Pos -> Pos
applyFlip1 (MkFlip a b c) (MkPos x y z) = MkPos 
  (if a then (negate x) else x)
  (if b then (negate y) else y)
  (if c then (negate z) else z)

Perm : Type
Perm = Fin 6

allPerms : List Perm
allPerms = toList (range {len=6})

applyPerm1 : Perm -> Pos -> Pos
applyPerm1 perm (MkPos x y z) = case perm of
  0 => MkPos x y z
  1 => MkPos y x z
  2 => MkPos x z y
  3 => MkPos y z x
  4 => MkPos z x y
  5 => MkPos z y x

data Orient 
  = MkOrient Flip Perm

Show Orient where
  show (MkOrient f p) = show f ++ "::" ++ show p

allOrients : List Orient
allOrients = [ MkOrient f p | f<-allFlips, p<-allPerms ]

applyOrient1 : Orient -> Pos -> Pos
applyOrient1 (MkOrient f p) = applyFlip1 f . applyPerm1 p

applyOrient : Orient -> List Pos -> List Pos
applyOrient orient = map (applyOrient1 orient)

--------------------------------------------------------------------------------
-- sets

setSize : SortedSet a -> Nat
setSize = length . SortedSet.toList

isectSize : Ord a => SortedSet a -> SortedSet a -> Nat
isectSize set1 set2 = setSize (intersection set1 set2)

--------------------------------------------------------------------------------
-- distance sets 

DistanceSet : Type
DistanceSet = SortedSet Int

distanceSet : Scan -> DistanceSet
distanceSet list = SortedSet.fromList $ map (\(p,q) => distSquare p q) (choose2 list)

--------------------------------------------------------------------------------
-- relative sets

RelativeSet : Type
RelativeSet = SortedSet Pos

-- NB. sorting is important here and in `f`, as the two scans
-- can have the same beacons in different order!
asymRelativeSet : Scan -> RelativeSet
asymRelativeSet list = SortedSet.fromList $ map (\(p,q) => q - p) (choose2 $ sort list)

--------------------------------------------------------------------------------
-- part 1

orientationCandidates : Scan -> Scan -> List (Orient,Nat)
orientationCandidates scan1 scan2 = [ (orient, fun orient)  | orient <- allOrients ] where

  rel1 : RelativeSet
  rel1 = asymRelativeSet scan1

  fun : Orient -> Nat
  fun orient = isectSize rel1 (asymRelativeSet $ applyOrient orient scan2)

-- we re-orient the second scan to be compatible with the first
-- this always return two possibilites, flipped wrt. to each other...
bestOrientations : Scan -> Scan -> (Orient,Orient)
bestOrientations scan1 scan2 = 
  case reverse (sortBy (comparing snd) $ orientationCandidates scan1 scan2) of
    (p::q::_) => (fst p, fst q)
    _         => fatal "bestOrientation: should not happen"

Shift : Type
Shift = Pos

-- returns the 
--  1) size of intersection 
--  2) the shift of scan2 vs. scan1 
--  3) and the union
scannerUnionSameOrient : Scan -> Scan -> (Nat,Pos,Scan)
scannerUnionSameOrient scan1 scan2 = (setSize isect, shift, SortedSet.toList final) where

  histo : Histogram Pos
  histo = histogram [ v - u | v <- scan2, u <- scan1 ]

  shift : Shift
  shift = case reverse $ sortBy (comparing snd) $ SortedMap.toList histo of
    []           => fatal "scannerUnionSameOrient: one of the inputs was empty"
    ((vec,_)::_) => vec

  rescan2 : Scan
  rescan2 = map (\x => x - shift) scan2

  set1, set2 : SortedSet Pos
  set1 = SortedSet.fromList scan1
  set2 = SortedSet.fromList rescan2

  isect, final : SortedSet Pos
  isect = SortedSet.intersection set1 set2
  final = SortedSet.union        set1 set2

scannerUnion : Scan -> Scan -> (Orient,Shift,Scan)
scannerUnion scan1 scan2 = case bestOrientations scan1 scan2 of
  (orientA,orientB) => 
    let (mA,shiftA,scanA) = scannerUnionSameOrient scan1 (applyOrient orientA scan2)
        (mB,shiftB,scanB) = scannerUnionSameOrient scan1 (applyOrient orientB scan2)
    in  if mA >= mB 
          then (orientA,shiftA,scanA) 
          else (orientB,shiftB,scanB)

scannerUnion_ : Scan -> Scan -> Scan
scannerUnion_ scan1 scan2 = case scannerUnion scan1 scan2 of
  (_,_,scan3) => scan3

findIndexLargest : Ord a => Vect (S n) a -> Fin (S n)
findIndexLargest (x::xs) = case xs of
  []     => FZ
  (_::_) => case findIndexLargest xs of
    k => if index k xs > x then (FS k) else FZ

reduceScans : {n : Nat} -> Vect n Scan -> IO Scan
reduceScans []                  = fatal "reduce: empty"
reduceScans [scan]              = pure scan
reduceScans [scan1,scan2]       = pure $ scannerUnion_ scan1 scan2
reduceScans (this::rest@(_::_)) = do
  let dthis = distanceSet this
  let iss   = map (\that => isectSize dthis (distanceSet that)) rest
  let k     = findIndexLargest iss 
  putStrLn $ "chosed #0 vs. #" ++ show (FS k) ++ " from [0.." ++ show (minus n 1) ++ "]"
  reduceScans $ scannerUnion_ this (index k rest) :: Vect.deleteAt k rest 

part1 : {n : Nat} -> Vect n Scan -> IO ()
part1 scans = do
  putStrLn $ "\npart 1"
  final <- reduceScans scans
  putStrLn $ "total number of beacons = " ++ (show $ length final)

--------------------------------------------------------------------------------
-- part 2 

findScanners : {n : Nat} -> Vect (S n) Scan -> IO (Vect n Shift)
findScanners [scan]              = pure []
findScanners [scan1,scan2]       = do
  let (_,shift,_) = scannerUnion scan1 scan2
  pure [negate shift]
findScanners (this::rest@(_::_)) = do
  let dthis = distanceSet this
  let iss   = map (\that => isectSize dthis (distanceSet that)) rest
  let k     = findIndexLargest iss 
  putStrLn $ "chosed #0 vs. #" ++ show (FS k) ++ " from [0.." ++ show (minus n 1) ++ "]"
  let (_,shift,new) = scannerUnion this (index k rest) 
  shifts <- findScanners (new :: Vect.deleteAt k rest)
  pure (negate shift::shifts)

part2 : {n : Nat} -> Vect (S n) Scan -> IO ()
part2 scans = do
  putStrLn $ "\npart 2"
  scanners <- findScanners scans
  -- mapM_ printLn scanners
  let maxdist = foldl max (the Int 0) [ manhDist s1 s2 | (s1,s2) <- Vect.choose2 scanners ]
  putStrLn $ "maximum distance of scanners = " ++ (show maxdist)

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input19"
  let scans = map parseScanner $ filter (not . null) $ forget (split null ls) 
  -- mapM_ printLn scans
  -- mapM_ printLn $ map distanceSet scans
  case scans of 
    []     => putStrLn "empty scanner list"
    (_::_) => do
  part1 (Vect.fromList scans)
  part2 (Vect.fromList scans)

