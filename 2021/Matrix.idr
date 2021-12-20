
-- this is very painful because Idris does not have eta-equality for pairs...

import Data.Fin
import Data.Vect

import Common
import Digit

--------------------------------------------------------------------------------
-- dimensions

public export
Dimensions : Type
Dimensions = Pair Nat Nat

public export
data PosDim : Dimensions -> Type where
  PosDimProof : {n1, m1 : Nat} -> (n = S n1) -> (m = S m1) -> PosDim (n,m)

public export
isPosDim : (dim : Dimensions) -> Maybe (PosDim dim)
isPosDim (S n1, S m1) = Just (PosDimProof Refl Refl)
isPosDim (_   , _   ) = Nothing

scalePos : (k1 : Nat) -> {n, n1 : Nat} -> (n = S n1) -> (l1 : Nat ** ((S k1) * n = S l1))
scalePos k1 eq0 =
  let eq = cong (\x => S k1 * x) eq0
  in  (plus n1 (mult k1 (S n1)) ** eq)

public export
scalePosDim : (k : Nat) -> {k1, n, m : Nat} -> {auto 0 prf : k = S k1} -> PosDim (n,m) -> PosDim (k * n, k * m)
scalePosDim k (PosDimProof eq1 eq2) = PosDimProof
  (rewrite prf in snd $ scalePos k1 eq1)
  (rewrite prf in snd $ scalePos k1 eq2)

--------------------------------------------------------------------------------
-- indices

public export
data Index : Dimensions -> Type where
  MkIndex : Fin n -> Fin m -> Index (n,m)

public export
implementation Eq (Index dim) where
  (==) (MkIndex i1 j1) (MkIndex i2 j2) = i1 == i2 && j1 == j2

public export
implementation Ord (Index dim) where
  compare (MkIndex i1 j1) (MkIndex i2 j2) = case compare i1 i2 of
    LT => LT
    GT => GT
    EQ => compare j1 j2

public export
implementation Show (Index dim) where
  show (MkIndex i j) = show (i,j)

topLeftCorner : {dim : Dimensions} -> {auto prf : PosDim dim} -> Index dim
topLeftCorner {dim = (n,m)} {prf = PosDimProof eq1 eq2} = rewrite eq1 in (rewrite eq2 in MkIndex FZ FZ)

bottomRightCorner : {dim : Dimensions} -> {auto prf : PosDim dim} -> Index dim
bottomRightCorner {dim = (n,m)} {prf = PosDimProof eq1 eq2} = rewrite eq1 in (rewrite eq2 in MkIndex last last)

--------------------------------------------------------------------------------
-- matrices

public export
Matrix : Dimensions -> Type -> Type
Matrix (n,m) t = Vect n (Vect m t)

public export
implementation [matrix] {0 dim : Dimensions} -> Functor (Matrix dim) where
  map {dim=(_,_)} f mat = map (map f) mat

-- Idris inference is shit
public export
mapMatrix : (a -> b) -> Matrix dim a -> Matrix dim b
mapMatrix = map @{matrix}

public export
mkVect : {dim : Nat} -> (Fin dim -> a) -> Vect dim a
mkVect = tabulate

public export
mkMatrix : {dim : Dimensions} -> (Index dim -> a) -> Matrix dim a
mkMatrix {dim=(n,m)} f = mkVect (\i => mkVect (\j => f (MkIndex i j)))

--------------------------------------------------------------------------------

namespace Matrix

  -- NB: you have to pattern match on `dim` because lack of eta equality in Idris...
  public export
  index : {0 dim : Dimensions} -> Index dim -> Matrix dim a -> a
  index {dim=(_,_)} (MkIndex i j) mat = Vect.index j $ Vect.index i mat

  public export
  updateAt : {0 dim : Dimensions} -> Index dim -> (a -> a) -> Matrix dim a -> Matrix dim a
  updateAt {dim=(_,_)} (MkIndex i j) f mat = Vect.updateAt i (Vect.updateAt j f) mat

  public export
  replaceAt : {0 dim : Dimensions} -> Index dim -> a -> Matrix dim a -> Matrix dim a
  replaceAt idx x = updateAt idx (const x)

  public export
  allIndices : (dim : Dimensions) -> List (Index dim)
  allIndices (n,m) = [ MkIndex i j | i <- toList range, j <- toList range ]

  public export
  findIndices : {dim : Dimensions} -> (a -> Bool) -> Matrix dim a -> List (Index dim)
  findIndices {dim=(_,_)} cond matrix = concat
    [ [ MkIndex i j | j <- findIndices cond row ] | (i,row) <- zip range matrix ]

--------------------------------------------------------------------------------
-- neighbours

namespace Dim1
  public export
  neighbours : {n : Nat} -> Fin n -> List (Fin n)
  neighbours {n = S Z        } FZ      = []
  neighbours {n = S (S Z)    } (FS FZ) = [FZ]
  neighbours {n = S (S m)    } (FZ   ) = [FS FZ]
  neighbours {n = S (S (S m))} (FS FZ) = [FZ, FS (FS FZ)]
  neighbours {n = S (S m)    } (FS k ) = map FS (neighbours k)

namespace Dim2

  -- 4 direct neighbours
  public export
  neighbours : {dim : Dimensions} -> Index dim -> List (Index dim)
  neighbours {dim=(_,_)} (MkIndex i j) = [ (MkIndex a j) | a <- Dim1.neighbours i ]
                                      ++ [ (MkIndex i b) | b <- Dim1.neighbours j ]

  -- 4 diagonal neighbours
  public export
  diagNeighbours : {dim : Dimensions} -> Index dim -> List (Index dim)
  diagNeighbours {dim=(_,_)} (MkIndex i j)
    = [ (MkIndex a b) | a <- Dim1.neighbours i, b <- Dim1.neighbours j ]

  -- all 8 neighbours
  public export
  allNeighbours : {dim : Dimensions} -> Index dim -> List (Index dim)
  allNeighbours ij = neighbours ij ++ diagNeighbours ij

--------------------------------------------------------------------------------
-- parsing

namespace Matrix

  parseLine : (Char -> Maybe a) -> (m : Nat) -> String -> Maybe (Vect m a)
  parseLine f m str = case allJust $ map f $ unpack str of
    Nothing => Nothing
    Just xs => fromListN m xs

  parseMatrix' : (Char -> Maybe a) -> (dim : Dimensions) -> List String -> Maybe (Matrix dim a)
  parseMatrix' f (n,m) ls = fromListN n =<< allJust (map (parseLine f m) ls)

  -- parseLine : (m : Nat) -> String -> Maybe (Vect m Digit)
  -- parseLine = parseLine' parseDigit

  -- parseDigitMatrix' : (dim : Dimensions) -> List String -> Maybe (Matrix dim Digit)
  -- parseDigitMatrix' = parseMatrix' (parseLine' parseDigit)

  export
  parseMatrix : (Char -> Maybe a) -> List String -> Maybe (dim : Dimensions ** Matrix dim a)
  parseMatrix f ls =
    let n = length ls
        m = case ls of { (l::_) => length l ; _ => 0 }
        dim = (n,m)
    in  case parseMatrix' f dim ls of
          Just mat => Just (dim ** mat)
          Nothing  => Nothing

  export
  parseDigitMatrix : List String -> Maybe (dim : Dimensions ** Matrix dim Digit)
  parseDigitMatrix = parseMatrix parseDigit

  printLine : (a -> String) -> {m : Nat} -> Vect m a -> IO ()
  printLine f vect = do
    putStrLn $ concat [ f (index j vect) | j <- toList (range {len=m}) ]

  export
  printMatrix : (a -> String) -> {dim : Dimensions} -> Matrix dim a -> IO ()
  printMatrix f {dim=(_,_)} mat = mapM_ (printLine f) mat

  export
  printDigitMatrix : {dim : Dimensions} -> Matrix dim Digit -> IO ()
  printDigitMatrix = printMatrix showDigit

--------------------------------------------------------------------------------
