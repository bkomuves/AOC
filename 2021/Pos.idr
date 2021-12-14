
-- 2d positions

import Data.List1
import Data.String

import Common

--------------------------------------------------------------------------------

data Pos
  = MkPos Int Int

xcoord, ycoord : Pos -> Int
xcoord (MkPos x _) = x
ycoord (MkPos _ y) = y

Eq Pos where
  (==) (MkPos x1 y1) (MkPos x2 y2) = (x1 == x2 && y1 == y2)

Ord Pos where
  compare (MkPos x1 y1) (MkPos x2 y2) = case compare x1 x2 of
    LT => LT
    GT => GT
    EQ => compare y1 y2

Show Pos where
  show (MkPos x y) = "(" ++ show x ++ "," ++ show y ++ ")"

(+) : Pos -> Pos -> Pos
(+) (MkPos x1 y1) (MkPos x2 y2) = MkPos (x1+x2) (y1+y2)

(-) : Pos -> Pos -> Pos
(-) (MkPos x1 y1) (MkPos x2 y2) = MkPos (x1-x2) (y1-y2)

parsePos : String -> Pos
parsePos str = case split (==',') (trim str) of
  (xxx:::[yyy]) => MkPos (cast xxx) (cast yyy)
  _             => fatal $ "cannot parse position: `" ++ show str ++ "`"

--------------------------------------------------------------------------------

