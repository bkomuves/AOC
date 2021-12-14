
import Data.List
import Data.List1
import Data.Fin
import Data.Vect
import Data.String

import Common

--------------------------------------------------------------------------------

Wire : Type
Wire = Fin 7

Segment : Type
Segment = Fin 7

Display : Type
Display = Vect 7 Bool

PartialDisplay : Type
PartialDisplay = Vect 7 (Maybe Bool)

--------------------------------------------------------------------------------
-- parsing

[display] Show Display where
  show v = pack [ (if index i v then chr (cast (finToNat i) + 49) else '.') | i <- toList (range {len=7}) ]

emptyDisplay : Display
emptyDisplay = replicate 7 False

record Entry' a where
  constructor MkEntry
  patterns : Vect 10 a
  output   : Vect  4 a

Entry : Type
Entry = Entry' Display

[entry] Show a => Show (Entry' a) where
  show (MkEntry ps os) =
    unwords (map show $ toList ps) ++ " | " ++
    unwords (map show $ toList os)

showEntry : Entry -> String
showEntry = show @{entry @{display}}

printEntry : Entry -> IO ()
printEntry = putStrLn. showEntry

Functor Entry' where
  map f (MkEntry ps os) = MkEntry (map f ps) (map f os)

Input : Type
Input = List Entry

parseDisplay : String -> Display
parseDisplay = foldl f emptyDisplay . unpack where
  f : Display -> Char -> Display
  f old ' ' = old
  f old c   = case let n = ord c in case natToFin (cast (n-97)) 7 of
                Just j => replaceAt j True old
                _      => fatal "parseDisplay: expecting a character between `a` and `g`"

parseLine_ : String -> Entry' String
parseLine_ str = case split (=='|') str of
  left:::[right] => MkEntry (unsafeFromListN 10 $ words left) (unsafeFromListN 4 $ words right)
  _              => fatal $ "parseLine_:\n" ++ str

parseLine : String -> Entry' Display
parseLine = map parseDisplay . parseLine_

parseInput : List String -> Input
parseInput = map parseLine

--------------------------------------------------------------------------------
-- part1

uniqueDigits : List Digit
uniqueDigits = [1,4,7,8]

uniqueCounts : List Nat
uniqueCounts =
  [ 2        -- digit 1 has 2 segments
  , 4        -- digit 4 has 4 segments
  , 3        -- digit 7 has 3 segments
  , 7        -- digit 8 has 7 segments
  ]

countTrues : Vect n Bool -> Nat
countTrues vec = sum $ map (\b => if b then 1 else 0) vec

isUnique : Display -> Bool
isUnique disp = Prelude.elem (countTrues disp) uniqueCounts

solve1 : Input -> Nat
solve1 input = sum [ countTrues $ map isUnique (entry.output) | entry <- input ]

--------------------------------------------------------------------------------
-- part2

-- mapping from wires to segments
Mapping : Type
Mapping = Vect 7 Segment

standardDisplay_ : Vect 10 String
standardDisplay_ = unsafeFromListN 10
  [ "abcefg"    -- 0
  , "cf"        -- 1
  , "acdeg"     -- 2
  , "acdfg"     -- 3
  , "bcdf"      -- 4
  , "abdfg"     -- 5
  , "abdefg"    -- 6
  , "acf"       -- 7
  , "abcdefg"   -- 8
  , "abcdfg"    -- 9
  ]

standardDisplay : Vect 10 Display
standardDisplay = map parseDisplay standardDisplay_

--------------------------------------------------------------------------------

namespace Vect
  public export
  remove1 : Vect (S n) a -> Vect (S n) (a, Vect n a)
  remove1 (x::xs@Nil   ) = (x,Nil) :: []
  remove1 (x::xs@(_::_)) = (x,xs ) :: map (\(y,ys) => (y,x::ys)) (remove1 xs)

namespace List
  public export
  remove1 : List a -> List (a, List a)
  remove1 []      = []
  remove1 (x::xs) = (x,xs) :: [ (y,x::ys) | (y,ys) <- remove1 xs ]

mapPartial : Vect k Segment -> 

allSegments : List Segment
allSegments = toList $ range {len=7}

isCompatible : Vect k Segment -> Bool
isCompatible Nil       = True
isCompatible partial_

backtrack : what -> (forall k. what -> Vect k Segment -> Bool) -> List Mapping
backtrack param cond = worker Nil allSegments where

  worker : {k : Nat} -> Vect k Segment -> List Segment -> List Mapping
  worker {k=7} final    []   = if cond param final then [final] else []
  worker {k=_} final    []   = fatal "this should not happen"
  worker       partial_ rems = if cond param partial_
    then       concat [ worker (x::partial_) xs | (x,xs) <- remove1 rems ]
    else []

{-
-- whether a given segment is possible output for a given wire
Possible : Type
Possible = (Vect 7 Bool)

-- all possibilities for all wires
Info : Type
Info = Vect 7 Possible

noInfo : Info
noInfo = replicate 7 (replicate 7 True)

-- given a display pattern, we return which digits is it compatible
-- according to our current knowledge
checkDigit : Display -> Info -> Possible
checkDigit display knowledge = 

solvePattern : Vect 10 Display -> Mapping
solvePattern

solveEntry : Entry -> Int
solveEntry = 
-}

solveEntry : Entry -> Int

solve2' : Input -> List Int
solve2' = map solveEntry

solve2 : Input -> Int
solve2 = sum . solve2'

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input8"
  let input = parseInput $ filterNonEmpty ls
  mapM_ printEntry input
  -- forM_ input $ \e => printLn $ map isUnique e.output
  putStrLn $ "part 1 = " ++ show (solve1 input)
