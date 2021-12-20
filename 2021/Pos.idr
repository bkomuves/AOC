
-- 2d positions

module Pos

import Data.List1
import Data.String

import Common

--------------------------------------------------------------------------------

public export
data Pos
  = MkPos Int Int

public export
xcoord, ycoord : Pos -> Int
xcoord (MkPos x _) = x
ycoord (MkPos _ y) = y

public export
Eq Pos where
  (==) (MkPos x1 y1) (MkPos x2 y2) = (x1 == x2 && y1 == y2)

public export
Ord Pos where
  compare (MkPos x1 y1) (MkPos x2 y2) = case compare x1 x2 of
    LT => LT
    GT => GT
    EQ => compare y1 y2

public export
Show Pos where
  show (MkPos x y) = "(" ++ show x ++ "," ++ show y ++ ")"

public export
(+) : Pos -> Pos -> Pos
(+) (MkPos x1 y1) (MkPos x2 y2) = MkPos (x1+x2) (y1+y2)

public export
(-) : Pos -> Pos -> Pos
(-) (MkPos x1 y1) (MkPos x2 y2) = MkPos (x1-x2) (y1-y2)

export
parsePos : String -> Pos
parsePos str = case split (==',') (trim str) of
  (xxx:::[yyy]) => MkPos (cast xxx) (cast yyy)
  _             => fatal $ "cannot parse position: `" ++ show str ++ "`"

--------------------------------------------------------------------------------

