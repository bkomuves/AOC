
{-

(* --- solution in Mathematica --- *)

cardPublic = 12320657 
doorPublic = 9659666  

cardPublicTest = 5764801  
doorPublicTest = 17807724 

a = MultiplicativeOrder[7, 20201227, {cardPublic}]
b = MultiplicativeOrder[7, 20201227, {doorPublic}]

(* test:  {a,b} = {8, 11}          *)
(* input: {a,b} = {6527904, 75188} *)

PowerMod[PowerMod[7, a, 20201227], b, 20201227]
PowerMod[PowerMod[7, b, 20201227], a, 20201227]

(* answer = 6421487 *)

-}

--------------------------------------------------------------------------------

{-# LANGUAGE Strict #-}

card_public = 12320657  :: Int
door_public = 9659666   :: Int

card_public_test = 5764801  :: Int 
door_public_test = 17807724 :: Int

subject = 7        :: Int
prime   = 20201227 :: Int

modp :: Int -> Int
modp x = mod x prime

-- the smallest integer m such that 7^m = target (mod prime)
multiplicativeOrder :: Int -> Int
multiplicativeOrder target = go 0 1 where
  go m x = if x == target then m else go (m+1) (modp (7*x))

-- b^k mod prime
powerMod :: Int -> Int -> Int 
powerMod b 0 = 1
powerMod b e = go 1 e where
  go :: Int -> Int -> Int
  go a e = case mod e 2 of { 0 -> yy ; 1 -> modp (b*yy) } where
    e1 = div e 2
    y  = powerMod b e1
    yy = modp (y*y)

solve :: (Int,Int) -> IO ()
solve (card_pub, door_pub) = do
  let a = multiplicativeOrder card_pub
  let b = multiplicativeOrder door_pub
  print (a,b)
  let x = powerMod (powerMod 7 a) b
  let y = powerMod (powerMod 7 b) a
  if x /= y 
    then putStrLn $ "fatal error in mathematics!"
    else putStrLn $ "the final answer = " ++ show x
main :: IO ()
main = do
  putStrLn "\nthe example:"
  solve (card_public_test, door_public_test)
  putStrLn "\nreal input:"
  solve (card_public, door_public)
