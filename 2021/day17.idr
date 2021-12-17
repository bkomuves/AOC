
-- import Common
import Data.SortedSet

--------------------------------------------------------------------------------

XVel, YVel : Type
XVel = Int
YVel = Int

XPos, YPos : Type
XPos = Int
YPos = Int

data Target 
  = MkTarget (XPos,XPos) (YPos,YPos)

exampleTarget : Target
exampleTarget = MkTarget (20,30) (-10,-5)

realTarget : Target
realTarget = MkTarget (88,125) (-157,-103)

--------------------------------------------------------------------------------

updateXVel : XVel -> XVel
updateXVel xvel = case compare xvel 0 of
  LT => xvel+1
  EQ => xvel
  GT => xvel-1

updateYVel : YVel -> YVel
updateYVel yvel = yvel - 1

xdistance : (XVel,Nat) -> XPos
xdistance (v,n) = go n 0 v where
  go : Nat -> XPos -> XVel -> XPos
  go Z     x _ = x
  go (S k) x v = go k (x+v) (updateXVel v)

ydistance : (YVel,Nat) -> YPos
ydistance (v,n) = go n 0 v where
  go : Nat -> YPos -> YVel -> YPos
  go Z     y _ = y
  go (S k) y v = go k (y+v) (updateYVel v)

--------------------------------------------------------------------------------
-- finding all possible shots

round : Double -> Int
round x = cast (x+0.5)

possibleXVels : (XPos,XPos) -> (YPos,YPos) -> List (XVel,Nat)
possibleXVels (a,b) (c,d) = go [] 1 where

  -- assuming yvel > 0 (reasonable, max always height >=0):
  -- after n = (2yvel + 1) steps, we are back to y=0
  -- at the point, the vertical speed is yvel-n = -yvel-1
  -- so if this is too big, it jumps over the target
  maxYVel : YVel
  maxYVel = -c 

  -- 0   <= yvel <= |c|
  -- |n| >= 2|c| + 3
  -- pos = n*y - n(n-1)/2
  -- 
  -- from:
  --   n*y      <= n*|c|
  --   n(n-1)/2 >  (n+1)|c|
  -- it follows that:
  --   pos < -|c|
  -- 
  -- solving for n:
  --   n > |c|+1 + sqrt(4c^2+12c+1) 
  --
  maxStepsI : Int
  maxStepsI = 
    let z : Double := cast (-c)
    in  - c + 1 + round (sqrt (4*z*z + 12*z + 1))

  maxSteps : Nat
  maxSteps = cast maxStepsI

  step : List Nat -> Nat -> XPos -> XVel -> List Nat
  step acc n x v = if v == 0
    then if x >= a && x <= b then reverse [n..max n maxSteps] ++ acc else acc
    else let acc1 = if x >= a && x <= b then n::acc else acc
         in  step acc1 (S n) (x+v) (updateXVel v)
  
  go : List (XVel,Nat) -> XVel -> List (XVel,Nat) 
  go acc xv = if xv > b 
    then reverse acc
    else let new = [ (xv,n) | n <- step [] 0 0 xv ] 
         in  go (new ++ acc) (xv + 1) 

possibleYVels : (YPos,YPos) -> Nat -> List YVel
possibleYVels (a,b) n0 = 
  let s0 = ydistance (0,n0)           -- s = s0 + yvel*n
      n  = cast n0
      y1 = div (a - s0 - (n-1)) n     -- signed `div` is not the mathematically correct one...
      y2 = div (b - s0 + (n-1)) n
  in  [ yv | yv<-[y1..y2], let d = ydistance (yv,n0), d >= a && d <= b ]

finalPos : ((XVel,YVel),Nat) -> (XPos,YPos)
finalPos ((xv,yv),n) = (xdistance (xv,n), ydistance (yv,n))

allHitting : Target -> List ((XVel,YVel),Nat)
allHitting (MkTarget ab cd) = 
  [ ((x,y),n) | (x,n) <- possibleXVels ab cd, y <- possibleYVels cd n ]

--------------------------------------------------------------------------------

maxHeight_ : (YVel,Nat) -> YPos
maxHeight_ (yv,n) = go 0 n 0 yv where
  go : YPos -> Nat -> YPos -> YVel -> YPos
  go maxy Z     y _ = max y maxy
  go maxy (S k) y v = go (max maxy y) k (y+v) (updateYVel v)

maxHeight : ((XVel,YVel),Nat) -> YPos
maxHeight ((xv,yv),n) = maxHeight_ (yv,n)

record Shoot where
  constructor MkShoot
  vel   : (XVel,YVel)
  steps : Nat
  hit   : (XPos,YPos)
  maxht : YPos

Show Shoot where
  show (MkShoot vel n hit maxht) = show vel ++ " --[" ++ show n ++ "]--> " ++ show hit ++ " (max=" ++ show maxht ++ ")"  

full : Target -> List Shoot
full tgt = [ MkShoot vel n (finalPos (vel,n)) (maxHeight (vel,n)) | (vel,n) <- allHitting tgt ]

--------------------------------------------------------------------------------
-- part 2 helpers

maximum : List Int -> Int
maximum []      = 0
maximum (x::xs) = foldl max x xs

unique : Ord a => List a -> List a
unique = SortedSet.toList . SortedSet.fromList

--------------------------------------------------------------------------------

main : IO ()
main = do
  let tgt = realTarget -- exampleTarget
      sols = full tgt
  -- mapM_ printLn sols
  putStrLn $ "maximum height achieved  = " ++ show (maximum $ map maxht sols)
  putStrLn $ "number of possible shots = " ++ show (length $ unique $ map vel $ sols)

