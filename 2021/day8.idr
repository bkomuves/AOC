
import Data.List
import Data.List1
import Data.Fin
import Data.Vect
import Data.String

import Common
import Digit

--------------------------------------------------------------------------------

Wire : Type
Wire = Fin 7

Segment : Type
Segment = Fin 7

Display : Type
Display = Vect 7 Bool

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
  f old c   = let n = ord c in case natToFin (cast (n-97)) 7 of
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
-- fuck it, let's just bruteforce; 7 factorial is only 5040

Perm : Nat -> Type
Perm n = Vect n (Fin n)

permutations : (n : Nat) -> List (Perm n)
permutations 0      = [[]]
permutations (S n1) = 
  [ Vect.insertAt k last (map Fin.weaken ps) 
  | ps <- permutations n1, k <- toList (Fin.range {len=S n1}) 
  ]

applyPerm1 : {n : Nat} -> Perm n -> Vect n a -> Vect n a
applyPerm1 perm old = map (\i => index (index i perm) old) range

applyPerm : Perm 7 -> Vect n Display -> Vect n Display
applyPerm perm = map (applyPerm1 perm)

findPerm : Vect 10 Display -> Maybe (Perm 7)
findPerm garbled = find compatible (permutations 7) where

  ref : List Display
  ref = sort $ toList standardDisplay

  compatible : Perm 7 -> Bool
  compatible perm = ref == sort (toList (applyPerm perm garbled))

whichDigit : Display -> Maybe (Fin 10)
whichDigit display = findIndex (==display) standardDisplay

unsafeWhichDigit : Display -> Fin 10
unsafeWhichDigit display = case whichDigit display of
  Nothing => fatal "unsafeWhichDigit: not a digit"
  Just d  => d

digitsToInt : Vect k (Fin 10) -> Int
digitsToInt digits = cast {from=String} {to=Int} $ concat $ map showDigit digits

solveEntry : Entry -> Int
solveEntry (MkEntry garbled fourdigit) = case findPerm garbled of
  Nothing   => fatal "cannot solve entry"
  Just perm => digitsToInt $ map unsafeWhichDigit (applyPerm perm fourdigit)

solve2' : Input -> List Int
solve2' = map solveEntry

solve2 : Input -> Int
solve2 = sum . solve2'

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input8"
  let input = parseInput $ filterNonEmpty ls
  -- mapM_ printEntry input
  putStrLn $ "part 1 = " ++ show (solve1 input)
  putStrLn $ "part 2 = " ++ show (solve2 input)
