
import Data.List
import Data.List1

import Data.Vect
import Data.String

import Common

--------------------------------------------------------------------------------
-- bingo board entries

data Mark 
  = Unmarked
  | Marked

Eq Mark where
  Unmarked == Unmarked = True
  Marked   == Marked   = True
  _        == _        = False

record Entry where
  constructor MkEntry
  mark   : Mark
  number : Int

Show Entry where
  show (MkEntry Unmarked n) = show n
  show (MkEntry Marked   n) = "(" ++ show n ++ ")"

isMarked : Entry -> Bool
isMarked e = (e.mark == Marked)

pristine : Int -> Entry
pristine n = MkEntry Unmarked n

--------------------------------------------------------------------------------
-- bingo boards

Board : Type
Board = Vect 5 (Vect 5 Entry)

readBoardList : List String -> List (List Int)
readBoardList = map (map cast . words)

readBoard : List String -> Maybe Board
readBoard ls = 
  let tmp = allJust $ map (fromListN 5) 
          $ map (map pristine) $ readBoardList ls 
  in  join (fromListN 5 <$> tmp)

unsafeReadBoard : List String -> Board
unsafeReadBoard ls = case readBoard ls of
  Nothing => fatal "unsafeReadBoard"
  Just b  => b

record Input n where
  constructor MkInput
  numbers : List Int
  boards  : Vect n Board

-- there is an extra newline at the end of the input, causing issues
notEof : List String -> Bool
notEof = not . null 

parseInput : List String -> (n : Nat ** Input n)
parseInput lines = 
  let (nls:::bls) = splitOn "" lines
      numbers = map cast $ forget $ split (==',') $ concat nls
      boards1 = map unsafeReadBoard $ filter notEof bls
  in  (length boards1 ** MkInput numbers (fromList boards1))

--------------------------------------------------------------------------------
-- winning condition

vecIsWon : Vect n Entry -> Bool
vecIsWon = and . map delay . map isMarked  

anyRowIsWon : Board -> Maybe (Fin 5)
anyRowIsWon board = findIndex id $ map vecIsWon board

anyColIsWon : Board -> Maybe (Fin 5)
anyColIsWon = anyRowIsWon . transpose 

data RowOrCol
  = Row (Fin 5)
  | Col (Fin 5)

Show RowOrCol where
  show (Row i) = "row=" ++ show i
  show (Col j) = "col=" ++ show j

boardIsWon : Board -> Maybe RowOrCol
boardIsWon board = case anyRowIsWon board of
  Just row => Just (Row row)
  Nothing  => Col <$> anyColIsWon board

gameIsWon : {n : Nat} -> Input n -> Maybe (Fin n, RowOrCol)
gameIsWon (MkInput _ boards) = worker $ zip range boards where
  worker : forall k. Vect k (Fin n, Board) -> Maybe (Fin n, RowOrCol)
  worker []           = Nothing
  worker ((j,b)::jbs) = case boardIsWon b of
    Nothing => worker jbs
    Just rc => Just (j,rc)

--------------------------------------------------------------------------------
-- part 1

drawEntry : Int -> Entry -> Entry
drawEntry n entry@(MkEntry mark k) = if n /= k 
  then entry
  else MkEntry Marked k

drawRow : Int -> Vect n Entry -> Vect n Entry
drawRow n = map (drawEntry n) 

drawBoard : Int -> Board -> Board
drawBoard n = map (drawRow n)

draw : Input n -> Input n
draw (MkInput numbers boards) = case numbers of
  []           => fatal "draw: empty draw"
  (this::rest) => MkInput rest (map (drawBoard this) boards)

unmarkeds : Board -> List Int
unmarkeds = map number . filter (not  . isMarked) . concat . map toList . toList 

solve1 : {n : Nat} -> Input n -> IO ()
solve1 = go where

  go : Input n -> IO ()
  go       (MkInput []        _     ) = fatal "empty draw"
  go input@(MkInput (this::_) boards) = do
    let input' = draw input
    case gameIsWon input' of
      Nothing    => go input'
      Just (j,_) => do
        let b = index j (input'.boards)
            u = unmarkeds b
        printLn $ this * (sum u)

--------------------------------------------------------------------------------
-- part 2

-- we ignore boards already won
gameIsWonFiltered : {n : Nat} -> Input n -> Vect n Bool -> List (Fin n, RowOrCol)
gameIsWonFiltered (MkInput _ boards) alreadyWon = worker $ zip3 range alreadyWon boards where

  worker : forall k. Vect k (Fin n, Bool, Board) -> List (Fin n, RowOrCol)
  worker []                = []
  worker ((j,done,b)::jbs) = case boardIsWon b of
    Nothing => worker jbs
    Just rc => if done
      then           worker jbs
      else (j,rc) :: worker jbs

solve2 : {n : Nat} -> Input n -> IO ()
solve2 input = go input (replicate n False) where

  go : Input n -> Vect n Bool -> IO ()
  go       (MkInput []        _     )  _         = fatal "empty draw"
  go input@(MkInput (this::_) boards) alreadyWon = do
    -- putStrLn $ "draw = " ++ show this
    let input' = draw input
    let wons : List (Fin n, RowOrCol)
        wons = gameIsWonFiltered input' alreadyWon 
    -- forM_ wons $ \(j,pos) => putStrLn $ "board #" ++ show j ++ " won at " ++ show pos
    let alreadyWon' = foldl (\w, (j,_) => replaceAt j True w) alreadyWon wons
    case and $ map delay alreadyWon' of
      False => go input' alreadyWon'
      True  => case wons of
        [(j,_)] => do
          putStrLn $ "last board = " ++ show j
          let b = index j (input'.boards)
              u = unmarkeds b
          printLn $ this * (sum u)
        _ => do
          putStrLn $ "problem: multiple last boards: " ++ show (map fst wons)

--------------------------------------------------------------------------------

main : IO ()
main = do
  lines <- readLines "input4"
  case parseInput lines of 
    (n ** input) => do 
      solve1 input
      solve2 input

