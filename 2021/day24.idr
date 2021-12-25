
import Data.Vect
import Data.List
import Data.List1
import Data.String

import Decidable.Equality

import Common

--------------------------------------------------------------------------------
-- registers

data Reg = W | X | Y | Z 

Eq Reg where
  (==) W W = True
  (==) X X = True
  (==) Y Y = True
  (==) Z Z = True
  (==) _ _ = False

Show Reg where
  show W = "W"
  show X = "X"
  show Y = "Y"
  show Z = "Z"

--------------------------------------------------------------------------------
-- values

Value : Type
Value = Int 

fromBool : Bool -> Value 
fromBool b = if b then 1 else 0

toBool : Value -> Bool
toBool x = (x /= 0)

eql : Value -> Value -> Value
eql x y = fromBool (x==y)

--------------------------------------------------------------------------------
-- instructions

data Arg reg
  = AReg reg
  | ALit Value

Functor Arg where
  map f (AReg r) = AReg (f r)
  map _ (ALit v) = ALit v

Eq reg => Eq (Arg reg) where
  (==) (AReg r1) (AReg r2) = (r1 == r2)
  (==) (ALit l1) (ALit l2) = (l1 == l2)
  (==) _         _         = False

Show reg => Show (Arg reg) where
  show (AReg reg) = show reg
  show (ALit lit) = show lit

data Instr reg
  = Inp reg
  | Add reg (Arg reg)
  | Mul reg (Arg reg) 
  | Div reg (Arg reg)   
  | Mod reg (Arg reg)   
  | Eql reg (Arg reg)
  | Set reg (Arg reg)

Functor Instr where
  map f instr = case instr of 
    Inp r   => Inp (f r)  
    Add r x => Add (f r) (map f x)
    Mul r x => Mul (f r) (map f x) 
    Div r x => Div (f r) (map f x)   
    Mod r x => Mod (f r) (map f x)   
    Eql r x => Eql (f r) (map f x)
    Set r x => Set (f r) (map f x)

Show reg => Show (Instr reg) where
  show instr = case instr of
    Inp reg     => "Inp " ++ show reg 
    Add reg arg => "Add " ++ show reg ++ " " ++ show arg
    Mul reg arg => "Mul " ++ show reg ++ " " ++ show arg 
    Div reg arg => "Div " ++ show reg ++ " " ++ show arg   
    Mod reg arg => "Mod " ++ show reg ++ " " ++ show arg   
    Eql reg arg => "Eql " ++ show reg ++ " " ++ show arg
    Set reg arg => "Set " ++ show reg ++ " " ++ show arg

target : Instr reg -> reg
target instr = case instr of
  Inp reg   => reg
  Add reg _ => reg
  Mul reg _ => reg
  Div reg _ => reg
  Mod reg _ => reg
  Eql reg _ => reg
  Set reg _ => reg

Program : Type -> Type
Program reg = List (Instr reg)

--------------------------------------------------------------------------------
-- evaluation

Memory : Type -> Type
Memory a = Vect 4 a

regToIdx : Reg -> Fin 4
regToIdx W = 0
regToIdx X = 1
regToIdx Y = 2
regToIdx Z = 3

set : Reg -> Memory a -> a -> Memory a
set reg mem val = replaceAt (regToIdx reg) val mem

get : Reg -> Memory a -> a
get reg mem = index (regToIdx reg) mem

load : Arg Reg -> Memory Value -> Value
load (AReg reg) mem = get reg mem
load (ALit val) _   = val

evalProgram : Foldable f => f (Instr Reg) -> List Value -> Memory Value -> Memory Value
evalProgram prg = worker (toList prg) where

  worker : Program Reg -> List Value -> Memory Value -> Memory Value
  worker Nil          _      mem = mem
  worker (this::rest) inputs mem = case this of
    Add reg arg => worker rest inputs $ set reg mem $ (+) (get reg mem) (load arg mem)
    Mul reg arg => worker rest inputs $ set reg mem $ (*) (get reg mem) (load arg mem)
    Div reg arg => worker rest inputs $ set reg mem $ div (get reg mem) (load arg mem)
    Mod reg arg => worker rest inputs $ set reg mem $ mod (get reg mem) (load arg mem)   
    Eql reg arg => worker rest inputs $ set reg mem $ eql (get reg mem) (load arg mem)
    Set reg arg => worker rest inputs $ set reg mem $                   (load arg mem)
    Inp reg     => case inputs of 
      (x::xs)     => worker rest xs (set reg mem x)
      []          => fatal "Inp: empty input" 

evalProgram_ : Foldable f => f (Instr Reg) -> List Value -> Memory Value 
evalProgram_ program input = evalProgram program input (replicate 4 0) where

--------------------------------------------------------------------------------
-- executing the program 

data Result = Valid | Invalid

Show Result where
  show Valid   = "valid"
  show Invalid = "invalid"

Digit : Type
Digit = Value

allDigits : List Digit
allDigits = [1..9]

runPrg' : Program Reg -> Vect 14 Digit -> (Result, Memory Value)
runPrg' prg input = 
  let mem = evalProgram_ prg (toList input)
  in  (if get Z mem == 0 then Valid else Invalid , mem)

runPrg : Program Reg -> Vect 14 Digit -> Result
runPrg prg input = fst (runPrg' prg input)

--------------------------------------------------------------------------------
-- parsing

parseReg : String -> Maybe Reg
parseReg "w" = Just W
parseReg "x" = Just X
parseReg "y" = Just Y
parseReg "z" = Just Z
parseReg _   = Nothing

parseReg_ : String -> Reg
parseReg_ s = case parseReg s of
  Just r  => r
  Nothing => fatal $ "parseReg_: not a registrer: `" ++ s ++ "`"

parseArg : String -> Arg Reg
parseArg s = case parseReg s of
  Just r  => AReg r
  Nothing => ALit (cast s)

parseInstr : String -> Instr Reg
parseInstr str = case split (==' ') str of
  "inp":::r::[]    => Inp (parseReg_ r)
  "add":::r::x::[] => Add (parseReg_ r) (parseArg x)
  "mul":::r::x::[] => Mul (parseReg_ r) (parseArg x)
  "div":::r::x::[] => Div (parseReg_ r) (parseArg x)
  "mod":::r::x::[] => Mod (parseReg_ r) (parseArg x)
  "eql":::r::x::[] => Eql (parseReg_ r) (parseArg x)
  "set":::r::x::[] => Set (parseReg_ r) (parseArg x)
  _                => fatal $ "invalid instruction: `" ++ str ++ "`"

-------------------------------------------------------------------------------

parseDigit : Char -> Digit
parseDigit ch = if (ch >= '0' && ch <= '9') 
  then cast (ord ch - 48)
  else fatal "parseDigit: not a digit"

parseModelNo : String -> Vect 14 Digit
parseModelNo str = 
  let ds = unpack str 
  in  case decEq (length ds) 14 of
        Yes prf => unsafeFromListN 14 $ map parseDigit ds
        No  _   => fatal "parseModelNo: expecting 14 digits"

-------------------------------------------------------------------------------
-- the parametrized standard block of 18 instructions

record Template where
  constructor MkTemplate
  param1 : Value
  param2 : Value
  divZBy : Value     -- always 1 or 26

Show Template where
  show (MkTemplate p q d) = "{ p=" ++ show p ++ " , q= " ++ show q ++ " , d=" ++ show d ++ " }"

matchTemplate : Vect 18 (Instr Reg) -> Maybe Template
matchTemplate vec = case vec of
  [ Inp W
  , Mul X (ALit 0 )
  , Add X (AReg Z )
  , Mod X (ALit 26)
  , Div Z (ALit d )
  , Add X (ALit p )
  , Eql X (AReg W )
  , Eql X (ALit 0 )
  , Mul Y (ALit 0 )
  , Add Y (ALit 25)
  , Mul Y (AReg X )
  , Add Y (ALit 1 ) 
  , Mul Z (AReg Y ) 
  , Mul Y (ALit 0 ) 
  , Add Y (AReg W ) 
  , Add Y (ALit q ) 
  , Mul Y (AReg X ) 
  , Add Z (AReg Y ) 
  ] => Just $MkTemplate p q d
  _ => Nothing

fromTemplate : Template -> Vect 18 (Instr Reg)
fromTemplate (MkTemplate p q d) = 
  [ Inp W
  , Mul X (ALit 0 )
  , Add X (AReg Z )
  , Mod X (ALit 26)
  , Div Z (ALit d )
  , Add X (ALit p )
  , Eql X (AReg W )
  , Eql X (ALit 0 )
  , Mul Y (ALit 0 )
  , Add Y (ALit 25)
  , Mul Y (AReg X )
  , Add Y (ALit 1 ) 
  , Mul Z (AReg Y ) 
  , Mul Y (ALit 0 ) 
  , Add Y (AReg W ) 
  , Add Y (ALit q ) 
  , Mul Y (AReg X ) 
  , Add Z (AReg Y ) 
  ]

mkTemplate : Value -> Value -> Value -> Vect 18 (Instr Reg)
mkTemplate p q d = fromTemplate $ MkTemplate p q d

matchProgram : Vect 252 (Instr Reg) -> Maybe (Vect 14 Template)
matchProgram = worker where

  worker : {k : Nat} -> Vect (18*k) (Instr Reg) -> Maybe (Vect k Template)
  worker {k=0   } []   = Just []
  worker {k=S k1} vec0 = 
    case matchTemplate (take 18 vec) of 
      Nothing    => Nothing
      Just this  => (this ::) <$> worker {k=k1} rem
    where
      vec : Vect (18+18*k1) (Instr Reg)
      vec = rewrite sym (multRightSuccPlus 18 k1) in vec0
      rem : Vect (18*k1) (Instr Reg)
      rem = drop 18 vec

-- reverse engineered block semantics
-- given an input and a pre-existing Z register value, returns the new Z register value
blockSemantics : Template -> Digit -> Value -> Value
blockSemantics (MkTemplate p q d) w z = 
  let z' = div z d 
  in  if (mod z 26 + p == w) 
        then z'
        else 26 * z' + w + q

-- simulated block semantics
evaluateBlock : Template -> Digit -> Value -> Value
evaluateBlock templ w z = get Z result where

  input : List Value
  input = [w] 

  mem : Vect 4 Value
  mem = [0,0,0,z]

  result : Vect 4 Value
  result = evalProgram (toList $ fromTemplate templ) input mem 

-- compare the theoretical with the simulated semantics
testSemantics : Template -> Digit -> Value -> Bool
testSemantics templ w z = (evaluateBlock templ w z == blockSemantics templ w z) 

-------------------------------------------------------------------------------
-- the input (useful for debugging)

{- 

my input
========

{ p= 13 , q=  6 , d=1  }
{ p= 11 , q= 11 , d=1  }
{ p= 12 , q=  5 , d=1  }
{ p= 10 , q=  6 , d=1  }
{ p= 14 , q=  8 , d=1  }
{ p= -1 , q= 14 , d=26 }
{ p= 14 , q=  9 , d=1  }
{ p=-16 , q=  4 , d=26 }
{ p= -8 , q=  7 , d=26 }
{ p= 12 , q= 13 , d=1  }
{ p=-16 , q= 11 , d=26 }
{ p=-13 , q= 11 , d=26 }
{ p= -6 , q=  6 , d=26 }
{ p= -6 , q=  1 , d=26 }

-}

myTemplateList : List Template
myTemplateList =
  [ MkTemplate ( 13) ( 6) (1 )
  , MkTemplate ( 11) (11) (1 )
  , MkTemplate ( 12) ( 5) (1 )
  , MkTemplate ( 10) ( 6) (1 )
  , MkTemplate ( 14) ( 8) (1 )
  , MkTemplate ( -1) (14) (26)
  , MkTemplate ( 14) ( 9) (1 )
  , MkTemplate (-16) ( 4) (26)
  , MkTemplate ( -8) ( 7) (26)
  , MkTemplate ( 12) (13) (1 )
  , MkTemplate (-16) (11) (26)
  , MkTemplate (-13) (11) (26)
  , MkTemplate ( -6) ( 6) (26)
  , MkTemplate ( -6) ( 1) (26)
  ] 

myTemplateVec : Vect 14 Template
myTemplateVec = Vect.fromList myTemplateList

--------------------------------------------------------------------------------
-- filling partial solutions

-- in case of Nothing, we guess the digit to write here
-- in case of Just we just fill it
fillDigit : Template -> Maybe Digit -> Value -> (Digit,Value)
fillDigit tmpl@(MkTemplate p q d) mb z = case mb of
  Just w  => (w, blockSemantics tmpl w z) 
  Nothing => let w = if d == 1 then 9 else (mod z 26 + p)
             in  (w, blockSemantics tmpl w z)

fillDigits : Vect k Template -> Vect k (Maybe Digit) -> Value -> (Vect k Digit, Value)
fillDigits []           []        z = ([],z)
fillDigits (this::rest) (mb::mbs) z = 
  let (w ,z1) = fillDigit  this mb  z
      (ws,z2) = fillDigits rest mbs z1
  in  (w::ws, z2)

isValidDigit : Digit -> Maybe Digit
isValidDigit d = if (d >= 1 && d <= 9) then Just d else Nothing

isValidNumber : Vect k Digit -> Maybe Integer
isValidNumber digits = map f $ allJust (map isValidDigit $ toList digits) where
  f : List Digit -> Integer
  f ds = cast (concatMap show ds)

toDigits : Integer -> List Digit
toDigits n = map (\c => ord c - 48) $ unpack $ show n

-------------------------------------------------------------------------------
-- part 1

-- use the given digits (in the places where the divisor is 1) to create a filling pattern
templateFilling : Vect n Template -> List Digit -> Vect n (Maybe Digit)
templateFilling []             _       = []
templateFilling (_    :: rest) []      = Nothing :: templateFilling rest []
templateFilling (this :: rest) (w::ws) = case this of 
  MkTemplate _ _ d => if d == 1 
    then Just w  :: templateFilling rest     ws
    else Nothing :: templateFilling rest (w::ws)

checkSolution : Vect 14 Template -> List Digit -> Maybe Integer
checkSolution tmpl digits = 
   let (ds,z) = fillDigits tmpl (templateFilling tmpl digits) 0
   in  if z /= 0 then Nothing else isValidNumber ds

solve1 : Vect 14 Template -> IO Integer 
solve1 tmpl = worker 99999 where

  worker : Integer -> IO Integer
  worker 11111 = fatal "solve1: no solution found"
  worker high5 = do
    case candidates of
      []      => worker (high5 - 1)
      (x::xs) => pure $ foldl max x xs
    where
      candidates : List Integer
      candidates = catMaybes [ checkSolution tmpl (toDigits (100*high5 + low)) | low<-[1..99] ]

-------------------------------------------------------------------------------
-- part 2

checkSolutionV2 : Vect 14 Template -> List Digit -> Maybe Integer
checkSolutionV2 tmpl digits = 
   let (ds,z) = fillDigits tmpl (templateFilling tmpl digits) 0
   in  if z /= 0 then Nothing else isValidNumber ds

solve2 : Vect 14 Template -> IO Integer 
solve2 tmpl = worker 11111 where

  worker : Integer -> IO Integer
  worker 99999 = fatal "solve2: no solution found"
  worker high5 = do
    case candidates of
      []      => worker (high5 + 1)
      (x::xs) => pure $ foldl min x xs
    where
      candidates : List Integer
      candidates = catMaybes [ checkSolutionV2 tmpl (toDigits (100*high5 + low)) | low<-[1..99] ]

-------------------------------------------------------------------------------

main : IO ()
main = do
  ls <- filterNonEmpty <$> readLines "input24" 
  let prgL = map parseInstr ls
      n    = length prgL
      prgV = Vect.fromList prgL

  case decEq (length prgL) 252 of
    No  _   => pure ()
    Yes prf => do
      case matchProgram (rewrite sym prf in prgV) of
        Nothing     => putStrLn "block template does not match the input program"
        Just templs => do
          putStrLn "\nblock template parameters:"
          mapM_ printLn templs

          sol <- solve1 templs
          putStrLn $ "\nanswer to part 1 = " ++ show sol
          putStr "sanity check: "
          let params = toDigits sol
          printLn $ fst <$> runPrg' prgL <$> (fromListN 14 params)

          sol <- solve2 templs
          putStrLn $ "\nanswer to part 2 = " ++ show sol
          putStr "sanity check: "
          let params = toDigits sol
          printLn $ fst <$> runPrg' prgL <$> (fromListN 14 params)
