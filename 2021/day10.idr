
import Data.Maybe
import Data.List
import Data.String
import Data.Vect

import Common

--------------------------------------------------------------------------------

data DelimType
  = Paren
  | Bracket
  | Brace
  | Angle

Show DelimType where
  show Paren   = "`()`"
  show Bracket = "`[]`"
  show Brace   = "`{}`"
  show Angle   = "`<>`"

Eq DelimType where
  (==) Paren   Paren   = True
  (==) Bracket Bracket = True
  (==) Brace   Brace   = True
  (==) Angle   Angle   = True
  (==) _       _       = False

data OpenClose
  = Open
  | Close

record Delim where
  constructor MkDelim
  delimType : DelimType
  openClose : OpenClose

parseDelim : Char -> Delim
parseDelim c = case c of
  '(' => MkDelim Paren   Open
  '[' => MkDelim Bracket Open
  '{' => MkDelim Brace   Open
  '<' => MkDelim Angle   Open
  ')' => MkDelim Paren   Close
  ']' => MkDelim Bracket Close
  '}' => MkDelim Brace   Close
  '>' => MkDelim Angle   Close
  _   => fatal $ "parseDelim: invalid character: `" ++ singleton c ++ "`"

--------------------------------------------------------------------------------

Subsystem : Type
Subsystem = List Delim

Input : Type
Input = List Subsystem

--------------------------------------------------------------------------------
-- parsing subsystems

Stack : Type
Stack = List DelimType

data Res : Type -> Type where
  Ok         : a         -> Res a
  Illegal    : DelimType -> Res a
  Incomplete : Stack     -> Res a

Show a => Show (Res a) where
  show (Ok         x) = "Ok "   ++ show x
  show (Illegal    t) = "Ill "  ++ show t
  show (Incomplete s) = "Incl " ++ show s

checkSubsystem : Subsystem -> Res ()
checkSubsystem = go [] where
  go : Stack -> List Delim -> Res ()
  go Nil     Nil     = Ok ()
  go (t::ts) Nil     = Incomplete (t::ts)
  go ts      (c::cs) = case c of
    MkDelim dt oc =>case oc of
      Open  => go (dt::ts) cs
      Close => case ts of
        Nil      => Illegal dt
        (t::ts') => if t==dt
          then go ts' cs
          else Illegal dt

--------------------------------------------------------------------------------
-- part 1

illegalScore : DelimType -> Nat
illegalScore Paren   = 3
illegalScore Bracket = 57
illegalScore Brace   = 1197
illegalScore Angle   = 25137

isIllegal : Res a -> Maybe DelimType
isIllegal (Illegal dt) = Just dt
isIllegal _            = Nothing

part1 : Input -> IO ()
part1 input = do
  putStrLn "\npart 1:"
  let res    = map checkSubsystem input
  let illegs = map illegalScore $ catMaybes $ map isIllegal res
  let answer = sum illegs
  -- putStrLn $ "checks   = " ++ show res
  -- putStrLn $ "illegals = " ++ show illegs
  putStrLn $ "answer   = " ++ show answer

--------------------------------------------------------------------------------
-- typesafe middle element silliness (for part 2), because we are in Idris after all

mutual
  data Odd : Nat -> Type where
    IsOdd : Even n -> Odd (S n)

  data Even : Nat -> Type where
    IsZero : Even Z
    IsEven : Odd n -> Even (S n)

plus1 : Either (Odd n) (Even n) -> Either (Odd (S n)) (Even (S n))
plus1 (Left  p) = Right (IsEven p)
plus1 (Right p) = Left  (IsOdd  p)

evenOrOdd : (n : Nat) -> Either (Odd n) (Even n)
evenOrOdd Z     = Right IsZero
evenOrOdd (S n) = plus1 (evenOrOdd n)

mutual
  oddHalf : Odd n -> Fin n
  oddHalf (IsOdd p) = case evenHalf p of
    Just half => weaken half
    Nothing   => FZ

  evenHalf : Even n -> Maybe (Fin n)
  evenHalf IsZero     = Nothing
  evenHalf (IsEven p) = Just $ FS (oddHalf p)

middleOf : Vect n a -> (prf : Odd n) -> a
middleOf vec prf = index (oddHalf prf) vec

--------------------------------------------------------------------------------
-- part 2

delimScore : DelimType -> Nat
delimScore Paren   = 1
delimScore Bracket = 2
delimScore Brace   = 3
delimScore Angle   = 4

stackScore : Stack -> Nat
stackScore = go 0 where
  go : Nat -> Stack -> Nat
  go acc Nil     = acc
  go acc (t::ts) = go (5*acc + delimScore t) ts

isIncomplete : Res a -> Maybe Stack
isIncomplete (Incomplete s) = Just s
isIncomplete _              = Nothing

part2 : Input -> IO ()
part2 input = do
  putStrLn "\npart 2:"
  let res    = map checkSubsystem input
  let compls = map stackScore $ catMaybes $ map isIncomplete res
  -- putStrLn $ "checks      = " ++ show res
  -- putStrLn $ "completions = " ++ show compls
  let sorted = sort compls
  case evenOrOdd (length sorted) of
    Right _   => fatal "even number of comletions"
    Left  prf =>  do
      let answer = middleOf (fromList sorted) prf
      putStrLn $ "answer      = " ++ show answer

--------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input10"
  let input = map (map parseDelim . unpack) ls
  part1 input
  part2 input
