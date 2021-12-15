
-- digits (these appear sometimes in AOC problems)

import Data.List
import Data.Vect
import Data.String

import Common

--------------------------------------------------------------------------------

public export
Digit : Type
Digit = Fin 10

public export
digitToNat : Digit -> Nat
digitToNat = finToNat

public export
digitToInt : Digit -> Int
digitToInt = cast . finToNat

public export
parseDigit : Char -> Maybe Digit
parseDigit c = natToFin (cast (ord c - 48)) 10

public export
unsafeParseDigit : Char -> Digit
unsafeParseDigit c = case parseDigit c of { Just d => d ; Nothing => fatal "unsafeParseDigit: not a digit" }

public export
digitToChar : Digit -> Char
digitToChar d = chr $ cast (finToNat d) + 48

public export
showDigit : Digit -> String
showDigit d = singleton $ digitToChar d

public export
[digit] Show Digit where show = showDigit

public export
allDigitsV : Vect 10 Digit
allDigitsV = range

public export
allDigitsL : List Digit
allDigitsL = toList allDigitsV

--------------------------------------------------------------------------------
