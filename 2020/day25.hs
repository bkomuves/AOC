
{-# LANGUAGE BangPatterns #-}

card_public = 11404017
door_public = 13768789

{-
a = MultiplicativeOrder[7, 20201227, {11404017}]
b = MultiplicativeOrder[7, 20201227, {13768789}]
11710225
8516638

PowerMod[PowerMod[7, a, 20201227], b, 20201227]
PowerMod[PowerMod[7, b, 20201227], a, 20201227]
18862163
-}

subject = 7        :: Int
prime   = 20201227 :: Int

modp x = mod x prime

iter :: Int -> (a -> a) -> (a -> a)
iter !n f !x = go n x where
  go !n !x 
    | n == 0    = x
    | otherwise = let !y = f x in go (n-1) y

transformN :: Int -> Int -> Int
transformN n = iter n step where

step :: Int -> Int
step x = mod (x*7) prime

card_public_test = 5764801
door_public_test = 17807724

main = do
  print $ modp $ subject^(8)
  print $ modp $ subject^(11)
  print $ modp $ subject^(8+11)

  print $ iter 8  step $ 1
  print $ iter 11 step $ 1
  print $ iter 11 step $ iter 8 step $ 1

  print $ iter 8  step $ 17807724
  print $ iter 11 step $ 5764801
