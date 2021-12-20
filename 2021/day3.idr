
import Data.List
import Data.Vect
import Data.String

import Common

%hide histogram

--------------------------------------------------------------------------------

data Digit
  = Zero
  | One

implementation Eq Digit where
  Zero == Zero = True
  One  == One  = True
  _    == _    = False

digitToInteger : Digit -> Integer
digitToInteger Zero = 0
digitToInteger One  = 1

parseDigit : Char -> Digit
parseDigit '0' = Zero
parseDigit '1' = One
parseDigit _   = fatal "expected a digit"

parseLine : String -> List Digit
parseLine str = map parseDigit $ unpack str

Input : Type
Input = (n : Nat ** List (Vect n Digit))

parseFile : List String -> Input
parseFile lines = case map parseLine lines of
  Nil     => fatal "empty input"
  (l::ls) => let n = length l in (n ** map (unsafeFromListN n) (l::ls))

--------------------------------------------------------------------------------

namespace List
  export
  fromBinary : List Digit -> Integer
  fromBinary = worker . reverse where
    worker : List Digit -> Integer
    worker Nil     = 0
    worker (d::ds) = digitToInteger d + 2 * worker ds

namespace Vect
  export
  fromBinary : Vect n Digit -> Integer
  fromBinary vec = fromBinary (toList vec)

--------------------------------------------------------------------------------
-- part 1

transpose : {n : Nat} -> List (Vect n a) -> Vect n (List a)
transpose Nil     = replicate n Nil
transpose (v::vs) = zipWith (::) v (transpose vs)

histogram : List Digit -> (Nat,Nat)
histogram = foldl f (0,0) where
  f : (Nat,Nat) -> Digit -> (Nat,Nat)
  f (n,m) Zero = (S n ,   m)
  f (n,m) One  = (  n , S m)

mostCommonDigit : (Nat,Nat) -> Digit
mostCommonDigit (n,m) = case compare n m of
  LT => One
  EQ => One
  GT => Zero

leastCommonDigit : (Nat,Nat) -> Digit
leastCommonDigit (n,m) = case compare n m of
  LT => Zero
  EQ => Zero
  GT => One

solve1 : Input -> IO Integer
solve1 (n ** input) = do
  let columns = transpose input
  let histo   = map histogram columns 
  let gamma   = fromBinary $ map mostCommonDigit  histo
  let epsilon = fromBinary $ map leastCommonDigit histo
  putStrLn $ "gamma   = " ++ show gamma
  putStrLn $ "epsilon = " ++ show epsilon
  pure (gamma*epsilon)

--------------------------------------------------------------------------------
-- part 2

oxygen : {n : Nat} -> (j : Nat) -> List (Vect n Digit) -> Integer
oxygen _ Nil      = fatal "oxygen: empty"
oxygen _ (v::Nil) = fromBinary v
oxygen j list     = case natToFin j n of
  Nothing => fatal "carbon: run out of digits"
  Just i  => let d = mostCommonDigit $ histogram $ index i $ transpose list
             in  oxygen (S j) $ filter (\v => index i v == d) list

carbon : {n : Nat} -> (j : Nat) -> List (Vect n Digit) -> Integer
carbon _ Nil      = fatal "oxygen: empty"
carbon _ (v::Nil) = fromBinary v
carbon j list     = case natToFin j n of
  Nothing => fatal "carbon: run out of digits"
  Just i  => let d = leastCommonDigit $ histogram $ index i $ transpose list
             in  carbon (S j) $ filter (\v => index i v == d) list

solve2 : Input -> IO Integer
solve2 (n ** input) = do
  let oxy = oxygen 0 input
  let co2 = carbon 0 input
  putStrLn $ "oxy = " ++ show oxy
  putStrLn $ "co2 = " ++ show co2
  pure (oxy*co2)

--------------------------------------------------------------------------------

main : IO ()
main = do
  lines <- readLines "input3"
  let input = parseFile $ filter (not . null) lines
  printLn =<< solve1 input
  printLn =<< solve2 input

