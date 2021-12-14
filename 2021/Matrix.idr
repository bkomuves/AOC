
import Data.Fin
import Data.Vect

import Common
import Digit

--------------------------------------------------------------------------------
-- matrices and indices

public export
Dimensions : Type
Dimensions = Pair Nat Nat

public export
Matrix : Dimensions -> Type -> Type
Matrix (n,m) t = Vect n (Vect m t)

public export
implementation [matrix] {0 dim : Dimensions} -> Functor (Matrix dim) where
  map {dim=(_,_)} f mat = map (map f) mat

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

  parseLine : (m : Nat) -> String -> Maybe (Vect m Digit)
  parseLine m str = case allJust $ map parseDigit $ unpack str of
    Nothing => Nothing
    Just xs => fromListN m xs

  parseDigitMatrix' : (dim : Dimensions) -> List String -> Maybe (Matrix dim Digit)
  parseDigitMatrix' (n,m) ls = fromListN n =<< allJust (map (parseLine m) ls)

  export
  parseDigitMatrix : List String -> Maybe (dim : Dimensions ** Matrix dim Digit)
  parseDigitMatrix ls =
    let n = length ls
        m = case ls of { (l::_) => length l ; _ => 0 }
        dim = (n,m)
    in  case parseDigitMatrix' dim ls of
          Just mat => Just (dim ** mat)
          Nothing  => Nothing

  printLine : {m : Nat} -> Vect m Digit -> IO ()
  printLine vect = do
    putStrLn $ concat [ showDigit (index j vect) | j <- toList (range {len=m}) ]

  export
  printDigitMatrix : {dim : Dimensions} -> Matrix dim Digit -> IO ()
  printDigitMatrix {dim=(_,_)} mat = mapM_ printLine mat

--------------------------------------------------------------------------------
