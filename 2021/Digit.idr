
-- digits (these appear sometimes in AOC problems)

import Data.List
import Data.Vect
import Data.String

import Common

--------------------------------------------------------------------------------

Digit : Type
Digit = Fin 10

digitToNat : Digit -> Nat
digitToNat = finToNat

digitToInt : Digit -> Int
digitToInt = cast . finToNat

parseDigit : Char -> Maybe Digit
parseDigit c = natToFin (cast (ord c - 48)) 10

unsafeParseDigit : Char -> Digit
unsafeParseDigit c = case parseDigit c of { Just d => d ; Nothing => fatal "unsafeParseDigit: not a digit" }

digitToChar : Digit -> Char
digitToChar d = chr $ cast (finToNat d) + 48

showDigit : Digit -> String
showDigit d = singleton $ digitToChar d

[digit] Show Digit where show = showDigit

allDigitsV : Vect 10 Digit
allDigitsV = range

allDigitsL : List Digit
allDigitsL = toList allDigitsV

--------------------------------------------------------------------------------
