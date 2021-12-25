
-- commonly used helper functions

module Common

import Data.Fin
import Data.So
import Data.List
import Data.Vect
import Data.String

import Data.SortedSet
import Data.SortedMap

import System.File

--------------------------------------------------------------------------------

public export
fatal : String -> a
fatal msg = assert_total $ idris_crash msg

--------------------------------------------------------------------------------
-- nat

-- iteration
public export
natRec : Nat -> (a -> a) -> a -> a
natRec Z     f x = x
natRec (S n) f x = f (natRec n f x)

public export
halve : Nat -> Nat
halve Z         = Z
halve (S Z)     = Z
halve (S (S n)) = S (halve n)

--------------------------------------------------------------------------------
-- Fin

public export
finToInt : Fin n -> Int
finToInt = cast . finToNat

public export
intToFin : Int -> (n : Nat) -> Maybe (Fin n)
intToFin k n = if k < 0 then Nothing else natToFin (cast k) n

public export
incFin : {n : Nat} -> Fin n -> Maybe (Fin n)
incFin {n=1      } FZ     = Nothing
incFin {n=S (S k)} FZ     = Just (FS FZ)
incFin {n=S k    } (FS j) = FS <$> (incFin j)

public export
decFin : Fin n -> Maybe (Fin n)
decFin FZ     = Nothing
decFin (FS k) = Just (weaken k)

public export
decFin' : Fin (S n) -> Maybe (Fin n)
decFin' FZ     = Nothing
decFin' (FS k) = Just k

-- including both starting and end point
public export
finRange : {n : Nat} -> Fin n -> Fin n -> List (Fin n)
finRange a b = if b < a then [] else go a where
  go : Fin n -> List (Fin n)
  go k = case incFin k of
    Nothing => [k]
    Just k1 => if k < b then k :: go k1 else [k]

-- including the starting point
public export
finRangeFrom : {n : Nat} -> Fin n -> List (Fin n)
finRangeFrom a = go a where
  go : Fin n -> List (Fin n)
  go k = case incFin k of
    Nothing => [k]
    Just k1 => k :: go k1 

-- including endpoint
public export
finRangeTo : {n : Nat} -> Fin n -> List (Fin n)
finRangeTo {n=S n1} b = finRange 0 b

public export
finDivMod : {k, n : Nat} -> Fin (k*n) -> (Fin k, Fin n)
finDivMod a0 =
  let a = finToNat a0
      q = div a n
      r = mod a n
 in case choose (q < k) of
      Right _   => fatal "finDivMod: should not happen /div"
      Left prf1 => case choose (r < n) of
        Right _   => fatal "finDivMod: should not happen /mod"
        Left prf2 => (natToFinLt q, natToFinLt r)

--------------------------------------------------------------------------------
-- strings

public export
readInt : String -> Int
readInt = cast

public export
safeTail : String -> Maybe String
safeTail str = assert_total $ if null str then Nothing else Just (strTail str)

public export
safeTail_ : String -> String
safeTail_ str = assert_total $ if null str then "" else strTail str

export
padRight : Nat -> String -> String
padRight n str = str ++ replicate (minus n $ length str) ' '

--------------------------------------------------------------------------------
-- lists

public export
allJust : List (Maybe a) -> Maybe (List a)
allJust = sequence

public export
fromListN : (n : Nat) -> List a -> Maybe (Vect n a)
fromListN 0     xs      = case xs of { Nil => Just Nil ; _ => Nothing }
fromListN (S n) (x::xs) = (x::) <$> fromListN n xs
fromListN _     _       = Nothing

public export
unsafeFromListN : (n : Nat) -> List a -> Vect n a
unsafeFromListN n xs = case fromListN n xs of
  Just vec => vec
  Nothing  => fatal "unsafeFromListN: lengths to not match"

export
nubSort : Ord a => List a -> List a
nubSort = SortedSet.toList . SortedSet.fromList

--------------------------------------------------------------------------------
-- vectors

public export
withIndices : {n : Nat} -> Vect n a -> Vect n (Fin n, a)
withIndices vec = zip range vec

--------------------------------------------------------------------------------
-- pars, triples

public export
pairs : List a -> List (a,a)
pairs Nil            = Nil
pairs (_::Nil)       = Nil
pairs (x::xs@(y::_)) = (x,y) :: pairs xs

public export
triples : List a -> List (a,a,a)
triples Nil               = Nil
triples (_::Nil)          = Nil
triples (_::_::Nil)       = Nil
triples (x::xs@(y::z::_)) = (x,y,z) :: triples xs

--------------------------------------------------------------------------------
-- choose 2

namespace List

  public export
  choose2 : List a -> List (a,a)
  choose2 []  = []
  choose2 [_] = []
  choose2 (x::xs) = [ (x,y) | y<-xs ] ++ choose2 xs

namespace Vect

  public export
  choose2 : Vect n a -> List (a,a)
  choose2 = List.choose2 . toList

--------------------------------------------------------------------------------
-- sortedmap

namespace SortedSet

  public export
  member : Ord k => k -> SortedSet k -> Bool
  member = contains

namespace SortedMap

  public export
  member : Ord k => k -> SortedMap k a -> Bool
  member k map = isJust (lookup k map)

  public export
  alter : Ord k => k -> (Maybe a -> Maybe a) -> SortedMap k a -> SortedMap k a
  alter key f map = case f (lookup key map) of
    Nothing => delete key   map
    Just y  => insert key y map

--------------------------------------------------------------------------------
-- histogram

namespace SortedMap

  public export
  Histogram : Type -> Type
  Histogram k = SortedMap k Nat
  
  public export
  insertWithPlus : (k,Nat) -> Histogram k -> Histogram k
  insertWithPlus (x,n) h = case lookup x h of
    Nothing => insert x  n    h
    Just m  => insert x (n+m) h
  
  public export
  histogram : Ord k => List k -> Histogram k
  histogram list = foldr ins empty list where
    ins : k -> SortedMap k Nat -> Histogram k
    ins x = insertWithPlus (x,1)  

--------------------------------------------------------------------------------
-- monads

public export
forM : (Monad m, Traversable f) => f a -> (a -> m b) -> m (f b)
forM = for

public export
forM_ : (Monad m, Foldable f) => f a -> (a -> m ()) -> m ()
forM_ what action = go (toList what) where
  go : List a -> m ()
  go []      = pure ()
  go (x::xs) = action x >> go xs

public export
mapM : (Monad m, Traversable f) => (a -> m b) -> f a -> m (f b)
mapM = flip forM

public export
mapM_ : (Monad m, Traversable f) => (a -> m ()) -> f a -> m ()
mapM_ = flip forM_

--------------------------------------------------------------------------------
-- reading from files

public export
filterNonEmpty : List String -> List String
filterNonEmpty = filter (not . null)

public export
readLines : String -> IO (List String)
readLines fname = do
  ei <- readFilePage 0 forever fname
  case ei of
    Left err => fatal $ "cannot read input file `" ++ fname ++ "`"
    Right (eof,lines) => pure $ map trim lines

--------------------------------------------------------------------------------
