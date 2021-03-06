
import Data.List

--------------------------------------------------------------------------------

readBinary :: [Bool] -> Int
readBinary []     = 0
readBinary (x:xs) = (if x then 1 else 0) + 2 * (readBinary xs) 

charToBool 'F' = False
charToBool 'B' = True
charToBool 'L' = False
charToBool 'R' = True

type SeatId = Int

decodeSeatId :: String -> SeatId
decodeSeatId = readBinary . map charToBool . reverse

seatPos :: SeatId -> (Int,Int)
seatPos seat = divMod seat 8

--------------------------------------------------------------------------------
{-

testData =
  [ "BFFFBBFRRR" 
  , "FFFBBBFRRR"
  , "BBFFBBFRLL"
  ]


test = do
  print [ (s,seatPos s) | s <- map decodeSeatId testData ]

-}
--------------------------------------------------------------------------------

main :: IO ()
main = do
  ws <- words <$> readFile "input5"
  let seats = map decodeSeatId ws
  putStrLn $ "maximum = " ++ show (maximum seats)
  let sorted = sort seats
  let holes = [ x+1 
              | x <- sorted , let y = x+1 , let z = y+1
              , not (elem y sorted)
              , elem z sorted
              ]
  putStrLn $ "holes   = " ++ show holes

