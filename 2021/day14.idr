
import Data.List
import Data.List1
import Data.String

import Data.SortedMap

import Common

--------------------------------------------------------------------------------

Letter : Type
Letter = Char

LPair : Type
LPair = Pair Letter Letter

Template : Type
Template = List Letter

data Rule
  = MkRule LPair Letter

Show Rule where
  show (MkRule (a,b) c) = singleton a ++ singleton b ++ " -> " ++ singleton c

parseRule : String -> Rule
parseRule str = case split (==' ') str of
  (abstr:::["->",cstr]) => case (unpack abstr, unpack cstr) of
    ([a,b],[c]) => MkRule (a,b) c
    _ => fatal "cannot parse insertion rule"
  _ => fatal "cannot parse insertion rule"

--------------------------------------------------------------------------------
-- rulesets

RuleSet : Type
RuleSet = SortedMap LPair Letter

insertRule : Rule -> RuleSet -> RuleSet
insertRule (MkRule ab c) = insert ab c

fromList : List Rule -> RuleSet
fromList = foldr insertRule empty

lookupRule : RuleSet -> (Letter,Letter) -> Maybe Letter
lookupRule table ab = lookup ab table

--------------------------------------------------------------------------------
-- parsing input

record Input where
  constructor MkInput
  template : Template
  rules    : List Rule

parseInput : List String -> Input
parseInput ls = case split null ls of
  ([template]:::(rules::_)) => MkInput (unpack $ trim template) (map parseRule rules)
  _ => fatal "cannot parse input"

--------------------------------------------------------------------------------
-- part 1

step : RuleSet -> Template -> Template
step ruleset = go where
  go : Template -> Template
  go Nil      = Nil
  go (z::Nil) = z::Nil
  go (x::xs@(y::_)) = case lookupRule ruleset (x,y) of
    Nothing => x :: go xs
    Just z  => x :: z :: go xs

part1 : Input -> IO ()
part1 (MkInput start rules) = do
  putStrLn "\npart 1"
  let ruleset = fromList rules
  let final = natRec 10 (step ruleset) start
  let histo = histogram final
  putStrLn $ "histogram = " ++ show histo
  let counts = sort $ map snd $ SortedMap.toList histo
  case counts of
    []     => fatal "this should never happen"
    (_::_) => do
      let least = head counts
      let most  = last counts
      putStrLn $ "answer = " ++ show (minus most least)

--------------------------------------------------------------------------------
-- part 2

PairCounts : Type
PairCounts = Histogram LPair

-- `fromListWith (+)` but that is missing from the SortedMap lib
fromListPlus : Ord k => List (k,Nat) -> Histogram k
fromListPlus list = foldr insertWithPlus empty list where

-- computing only the first pair
stepFirst : RuleSet -> LPair -> LPair
stepFirst ruleset ab@(a,_) = case lookup ab ruleset of { Nothing => ab ; Just c => (a,c) }

-- computing only the last pair
stepLast : RuleSet -> LPair -> LPair
stepLast ruleset ab@(_,b) = case lookup ab ruleset of { Nothing => ab ; Just c => (c,b) }

stepV2 : RuleSet -> PairCounts -> PairCounts
stepV2 ruleset = fromListPlus . concatMap worker . SortedMap.toList where
  worker : (LPair,Nat) -> List (LPair,Nat)
  worker (ab@(a,b), n) = case lookup ab ruleset of
    Nothing => [ ((a,b),n) ]
    Just c  => [ ((a,c),n) , ((c,b),n) ]

-- NB. we compute every letter twice, expecpt the very first and very last letters
sumPairHisto : PairCounts -> Histogram Letter
sumPairHisto = fromListPlus . concatMap worker . SortedMap.toList where
  worker : (LPair,Nat) -> List (Letter,Nat)
  worker ((a,b),n) = [(a,n),(b,n)]

isNonEmpty : (xs : List a) -> Maybe (NonEmpty xs)
isNonEmpty []     = Nothing
isNonEmpty (_::_) = Just IsNonEmpty

div2 : Nat -> Integer
div2 n = div (natToInteger n) 2

part2 : Input -> IO ()
part2 (MkInput start rules) = do
  putStrLn "\npart 2"
  let ruleset = fromList rules
  let n = 40
  let ps = pairs start
  case isNonEmpty ps of
    Nothing  => fatal "empty starting template"
    Just prf => do
      let paircnts  = natRec n (stepV2    ruleset) (histogram ps)
          (start,_) = natRec n (stepFirst ruleset) (head ps)
          (_ , end) = natRec n (stepLast  ruleset) (last ps)
          doubleCnt = insertWithPlus (start,1) $ insertWithPlus (end,1) $ sumPairHisto paircnts
          final     = map div2 doubleCnt       -- NB. we double counted everything!
      putStrLn $ "histogram = " ++ show final
      let counts = sort $ map snd $ SortedMap.toList final
      case isNonEmpty counts of
        Nothing  => fatal "this should never happen"
        Just prf => do
          let least = head counts
          let most  = last counts
          putStrLn $ "answer = " ++ show (most - least)

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- readLines "input14"
  let input = parseInput ls
  -- printLn (input.template)
  -- printLn (input.rules)
  part1 input
  part2 input

