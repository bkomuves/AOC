
-- 3D positions

module Pos3D

import Data.List1
import Data.String

import Common

--------------------------------------------------------------------------------

public export
data Pos 
  = MkPos Int Int Int

public export
Eq Pos where
  (==) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = (x1==x2) && (y1==y2) && (z1==z2)

public export
Ord Pos where
  compare (MkPos x1 y1 z1) (MkPos x2 y2 z2) = case compare x1 x2 of
    LT => LT
    GT => GT
    EQ => case compare y1 y2 of
      LT => LT
      GT => GT
      EQ => compare z1 z2

public export
Show Pos where
  show (MkPos x y z) = "( " ++ show x ++ " , " ++ show y ++ " , " ++ show z ++ " )"

infixl 6 +
public export
(+) : Pos -> Pos -> Pos
(+) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = MkPos (x1+x2) (y1+y2) (z1+z2)

infixl 6 -
public export
(-) : Pos -> Pos -> Pos
(-) (MkPos x1 y1 z1) (MkPos x2 y2 z2) = MkPos (x1-x2) (y1-y2) (z1-z2)

public export
negate : Pos -> Pos
negate (MkPos x y z) = MkPos (-x) (-y) (-z)

public export
absSquare : Pos -> Int
absSquare (MkPos x y z) = x*x + y*y + z*z

public export
distSquare : Pos -> Pos -> Int
distSquare (MkPos x1 y1 z1) (MkPos x2 y2 z2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2)

public export
manhDist : Pos -> Pos -> Int
manhDist (MkPos x1 y1 z1) (MkPos x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

public export
parsePos : String -> Maybe Pos
parsePos str = case split (==',') (trim str) of 
  x:::y::z::[] => Just $ MkPos (cast x) (cast y) (cast z)
  _            => Nothing

public export
parsePos_ : String -> Pos
parsePos_ str = case parsePos str of
  Just pos => pos
  Nothing  => fatal "cannot parse 3D position"

--------------------------------------------------------------------------------

