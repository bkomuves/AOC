
import Data.List
import Data.List1
import Data.Vect
import Data.Bits
import Data.DPair
import Data.String

import Common

--------------------------------------------------------------------------------

data Bit
  = B0
  | B1

Eq Bit where
  (==) B0 B0 = True
  (==) B1 B1 = True
  (==) _  _  = False

Show Bit where
  show B0 = "0"
  show B1 = "1"

Bits : Type
Bits = List Bit

bitsToString : Bits -> String
bitsToString Nil     = ""
bitsToString (b::bs) = show b ++ bitsToString bs

[bits] Show Bits where
  show bits = "`" ++ bitsToString bits ++ "`"

parseBit : Char -> Bit
parseBit '0' = B0
parseBit '1' = B1
parseBit _   = fatal "parseBit"

bitsFromString : String -> Bits
bitsFromString = map parseBit . unpack . trim

--------------------------------------------------------------------------------

bitValue : Bit -> Int
bitValue B0 = 0
bitValue B1 = 1

-- little endian
fromBitVecLE : Vect n Bit -> Int
fromBitVecLE Nil     = 0
fromBitVecLE (b::bs) = bitValue b + 2 * fromBitVecLE bs

-- big endian
fromBitVec : Vect n Bit -> Int
fromBitVec = fromBitVecLE . reverse

fromBitList : List Bit -> Int
fromBitList list = fromBitVec (fromList list)

natFromBitVec : Vect n Bit -> Nat
natFromBitVec = cast . fromBitVec

--------------------------------------------------------------------------------

Nibble : Type
Nibble = Int

parseHexChar : Char -> Nibble
parseHexChar c = if c >= '0' && c <= '9'
  then ord c - 48
  else if c >= 'A' && c <= 'F'
    then ord c - 65 + 10
    else fatal "parseHexChar: not a valid hex char"

myTestBit : Nibble -> (k : Nat) -> {auto 0 prf : LT k 4} -> {auto 0 triv : LTE 4 64} -> Bit
myTestBit nibble k = if testBit nibble (Element k (transitive prf triv)) then B1 else B0

nibbleToBits : Nibble -> Bits
nibbleToBits n =
  [ myTestBit n 3
  , myTestBit n 2
  , myTestBit n 1
  , myTestBit n 0
  ]

nibblesToList : List Nibble -> Bits
nibblesToList = concatMap nibbleToBits

parseHexString : String -> Bits
parseHexString = nibblesToList . map parseHexChar . unpack . trim

--------------------------------------------------------------------------------
-- parser combinator monad

data Parser : Type -> Type where
  MkP : (Bits -> Maybe (a, Bits)) -> Parser a

runP : Parser a -> Bits -> Maybe (a, Bits)
runP (MkP p) = p

Functor Parser where
  map f (MkP g) = MkP $ \bits => case g bits of
    Nothing        => Nothing
    Just (x, rest) => Just (f x, rest)

Applicative Parser where
  pure x  = MkP $ \bits => Just (x,bits)
  (MkP p) <*> (MkP q) = MkP $ \bits0 => case p bits0 of
    Nothing         => Nothing
    Just (f, bits1) => case q bits1 of
      Nothing         => Nothing
      Just (x, bits2) => Just (f x, bits2)

Monad Parser where
  (MkP p) >>= h = MkP $ \bits0 => case p bits0 of
    Nothing         => Nothing
    Just (x, bits1) => case runP (h x) bits1 of
      Nothing         => Nothing
      Just (y, bits2) => Just (y, bits2)

fail : Parser a
fail = MkP $ \_ => Nothing

Alternative Parser where
  empty = fail
  (MkP p) <|> q = MkP $ \bits => case p bits of
    Just (x, rest) => Just (x, rest)
    Nothing        => runP q bits

--------------------------------------------------------------------------------
-- basic parsers

nextBit : Parser Bit
nextBit = MkP $ \bits => case bits of
  Nil     => Nothing
  (b::bs) => Just (b,bs)

repeat : (n : Nat) -> Parser a -> Parser (Vect n a)
repeat Z     _ = pure Nil
repeat (S k) p = (::) <$> p <*> repeat k p

consume : (n : Nat) -> Parser (Vect n Bit)
consume n = repeat n nextBit

mutual

  some : Parser a -> Parser (List1 a)
  some p = do { x <- p ; xs <- many p ; pure (x:::xs) }

  some_ : Parser a -> Parser (List a)
  some_ p = forget <$> some p

  many : Parser a -> Parser (List a)
  many p = some_ p <|> pure Nil

repeat_ : Nat -> Parser a -> Parser (List a)
repeat_ n p = toList <$> repeat n p

--------------------------------------------------------------------------------
-- packets

data Tag
  = Sum       -- 0
  | Prod      -- 1
  | Min       -- 2
  | Max       -- 3
  --
  | Gt        -- 5
  | Lt        -- 6
  | Eq        -- 7

Show Tag where
  show Sum  = "Sum"
  show Prod = "Prod"
  show Min  = "Min"
  show Max  = "Max"
  show Gt   = "Gt"
  show Lt   = "Lt"
  show Eq   = "Eq"

parseTag : Nat -> Maybe Tag
parseTag tag = case tag of
  0 => Just Sum
  1 => Just Prod
  2 => Just Min
  3 => Just Max
  4 => Nothing
  5 => Just Gt
  6 => Just Lt
  7 => Just Eq
  _ => fatal "parseTag: expecting a number between 0..7"

mutual

  record Packet where
    constructor MkPacket
    version : Nat
    type    : RawPacket

  data RawPacket : Type where
    Lit : Nat -> RawPacket
    Op  : Tag -> List Packet -> RawPacket

mutual

  packet : Parser Packet
  packet = do
    ver <- natFromBitVec <$> consume 3
    raw <- rawPacket
    pure $ MkPacket ver raw

  rawPacket : Parser RawPacket
  rawPacket = do
    mbtag <- (parseTag . natFromBitVec) <$> consume 3
    case mbtag of
      Nothing  => literal
      Just tag => operator tag

  litPayload : Parser (List Bit)
  litPayload = do
    (flag::bits) <- consume 5
    let nib : List Bit
        nib = toList bits
    case flag of
      B0 => pure nib
      B1 => do { xs <- litPayload ; pure (nib++xs) }

  literal : Parser RawPacket
  literal = (Lit . cast . fromBitList) <$> litPayload

  operator : Tag -> Parser RawPacket
  operator tag = do
    type <- nextBit
    case type of
      B0 => Op tag <$> subPacketsType0
      B1 => Op tag <$> subPacketsType1

  subPacketsType0 : Parser (List Packet)
  subPacketsType0 = do
    n <- natFromBitVec <$> consume 15
    bits <- consume n
    case runP (some_ packet) (toList bits) of
      Nothing          => fail
      Just (list,rest) => case rest of
        Nil  => pure list
        _::_ => fail

  subPacketsType1 : Parser (List Packet)
  subPacketsType1 = do
    n <- natFromBitVec <$> consume 11
    repeat_ n packet

--------------------------------------------------------------------------------

parse : String -> Maybe Packet
parse str = case (runP packet $ parseHexString str) of
  Nothing         => Nothing
  Just (packet,_) => Just packet       -- snd <$> (...) does not typecheck?!

--------------------------------------------------------------------------------

ex0, ex1, ex2, ex3, ex4, ex5, ex6 : String
ex0 = "D2FE28"
ex1 = "38006F45291200"
ex2 = "EE00D40C823060"
ex3 = "8A004A801A8002F478"
ex4 = "620080001611562C8802118E34"
ex5 = "C0015000016115A2E0802F182340"
ex6 = "A0016C880162017C3686B18A3D4780"

--------------------------------------------------------------------------------
-- part 1

solve1 : Packet -> Nat
solve1 = goPkt where

  mutual

    goPkt : Packet -> Nat
    goPkt (MkPacket ver raw) = cast ver + goRaw raw

    goRaw : RawPacket -> Nat
    goRaw (Lit _       ) = 0
    goRaw (Op _ packets) = sum (map goPkt packets)

--------------------------------------------------------------------------------
-- part 2

evalOp : Tag -> List Integer -> Integer
evalOp Sum  args    = sum     args
evalOp Prod args    = product args
evalOp Min  (a::as) = foldl min a as
evalOp Max  (a::as) = foldl max a as
evalOp Gt   [a,b]   = if a >  b then 1 else 0
evalOp Lt   [a,b]   = if a <  b then 1 else 0
evalOp Eq   [a,b]   = if a == b then 1 else 0
evalOp tag  args    = fatal $ "evalOp: invalid tag/arity combination: " ++ show tag ++ " " ++ show (length args)

eval : Packet -> Integer
eval (MkPacket ver raw) = case raw of
  Lit value   => cast value
  Op tag args => evalOp tag $ map eval args

--------------------------------------------------------------------------------

main : IO ()
main = do
  hex <- unlines <$> readLines "input16"
  case parse hex of
    Nothing     => fatal "cannot parse hex string to a valid packet"
    Just packet => do
      putStrLn $ "answer to part 1 = " ++ show (solve1 packet)
      putStrLn $ "answer to part 2 = " ++ show (eval   packet)
