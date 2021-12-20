
-- minimalistic parser combinators

module Parser

import Data.List
import Data.List1
import Data.Vect

--------------------------------------------------------------------------------
-- parser combinator monad

public export
data GenParser : Type -> Type -> Type where
  MkP : (List tok -> Maybe (a, List tok)) -> GenParser tok a

public export
runParser : GenParser tok a -> List tok -> Maybe (a, List tok)
runParser (MkP p) = p

public export
Functor (GenParser tok) where
  map f (MkP g) = MkP $ \cs => case g cs of
    Nothing        => Nothing
    Just (x, rest) => Just (f x, rest)

public export
Applicative (GenParser tok) where
  pure x  = MkP $ \cs => Just (x,cs)
  (MkP p) <*> (MkP q) = MkP $ \cs0 => case p cs0 of
    Nothing         => Nothing
    Just (f, cs1) => case q cs1 of
      Nothing         => Nothing
      Just (x, cs2) => Just (f x, cs2)

public export
Monad (GenParser tok) where
  (MkP p) >>= h = MkP $ \cs0 => case p cs0 of
    Nothing         => Nothing
    Just (x, cs1) => case runParser (h x) cs1 of
      Nothing         => Nothing
      Just (y, cs2) => Just (y, cs2)

public export
fail : GenParser tok a
fail = MkP $ \_ => Nothing

public export
Alternative (GenParser tok) where
  empty = fail
  (MkP p) <|> q = MkP $ \cs => case p cs of
    Just (x, rest) => Just (x, rest)
    Nothing        => runParser q cs

--------------------------------------------------------------------------------

public export
anyToken : GenParser tok tok
anyToken = MkP $ \cs => case cs of
  Nil     => Nothing
  (c::cs) => Just (c,cs)

public export
satisfy : (tok -> Bool) -> GenParser tok tok
satisfy f = MkP $ \cs => case cs of
  Nil     => Nothing
  (c::cs) => if f c then Just (c,cs) else Nothing

--------------------------------------------------------------------------------

public export
repeat : (n : Nat) -> GenParser tok a -> GenParser tok (Vect n a)
repeat Z     _ = pure Nil
repeat (S k) p = (::) <$> p <*> repeat k p

public export
consume : (n : Nat) -> GenParser tok (Vect n tok)
consume n = repeat n anyToken

mutual

  public export
  some : GenParser tok a -> GenParser tok (List1 a)
  some p = do { x <- p ; xs <- many p ; pure (x:::xs) }

  public export
  some_ : GenParser tok a -> GenParser tok (List a)
  some_ p = forget <$> some p

  public export
  many : GenParser tok a -> GenParser tok (List a)
  many p = some_ p <|> pure Nil

public export
repeat_ : Nat -> GenParser tok a -> GenParser tok (List a)
repeat_ n p = toList <$> repeat n p

--------------------------------------------------------------------------------
