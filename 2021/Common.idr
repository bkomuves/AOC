
-- commonly used helper functions

import Data.List
import Data.Vect
import Data.String

import System.File

--------------------------------------------------------------------------------

fatal : String -> a
fatal msg = assert_total $ idris_crash msg

--------------------------------------------------------------------------------
-- strings

readInt : String -> Int
readInt = cast

safeTail : String -> Maybe String
safeTail str = assert_total $ if null str then Nothing else Just (strTail str)

safeTail_ : String -> String
safeTail_ str = assert_total $ if null str then "" else strTail str

--------------------------------------------------------------------------------
-- lists

allJust : List (Maybe a) -> Maybe (List a)
allJust = sequence

fromListN : (n : Nat) -> List a -> Maybe (Vect n a)
fromListN 0     xs      = case xs of { Nil => Just Nil ; _ => Nothing }
fromListN (S n) (x::xs) = (x::) <$> fromListN n xs
fromListN _     _       = Nothing

unsafeFromListN : (n : Nat) -> List a -> Vect n a
unsafeFromListN n xs = case fromListN n xs of
  Just vec => vec
  Nothing  => fatal "unsafeFromListN: lengths to not match"

--------------------------------------------------------------------------------
-- pars, triples

pairs : List a -> List (a,a)
pairs Nil            = Nil
pairs (_::Nil)       = Nil
pairs (x::xs@(y::_)) = (x,y) :: pairs xs

triples : List a -> List (a,a,a)
triples Nil               = Nil
triples (_::Nil)          = Nil
triples (_::_::Nil)       = Nil
triples (x::xs@(y::z::_)) = (x,y,z) :: triples xs

--------------------------------------------------------------------------------
-- monads

forM : (Monad m, Traversable f) => f a -> (a -> m b) -> m (f b)
forM = for

forM_ : (Monad m, Foldable f) => f a -> (a -> m ()) -> m ()
forM_ what action = go (toList what) where
  go : List a -> m ()
  go []      = pure ()
  go (x::xs) = action x >> go xs

mapM : (Monad m, Traversable f) => (a -> m b) -> f a -> m (f b)
mapM = flip forM

mapM_ : (Monad m, Traversable f) => (a -> m ()) -> f a -> m ()
mapM_ = flip forM_

--------------------------------------------------------------------------------
-- reading from files

filterNonEmpty : List String -> List String
filterNonEmpty = filter (not . null)

readLines : String -> IO (List String)
readLines fname = do
  ei <- readFilePage 0 forever fname
  case ei of
    Left err => fatal $ "cannot read input file `" ++ fname ++ "`"
    Right (eof,lines) => pure $ map trim lines

--------------------------------------------------------------------------------