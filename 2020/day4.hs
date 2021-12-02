
import Data.Char
import Data.List.Split

type Passport = [(String,String)]

--------------------------------------------------------------------------------

possibleFields = 
  [ "byr"  -- (Birth Year)
  , "iyr"  -- (Issue Year)
  , "eyr"  -- (Expiration Year)
  , "hgt"  -- (Height)
  , "hcl"  -- (Hair Color)
  , "ecl"  -- (Eye Color)
  , "pid"  -- (Passport ID)
  , "cid"  -- (Country ID)
  ]

requiredFields = 
  [ "byr"  -- (Birth Year)
  , "iyr"  -- (Issue Year)
  , "eyr"  -- (Expiration Year)
  , "hgt"  -- (Height)
  , "hcl"  -- (Hair Color)
  , "ecl"  -- (Eye Color)
  , "pid"  -- (Passport ID)
  ]

checkFields :: Passport -> Passport
checkFields p = case [ k | (k,v) <- p , not (elem k possibleFields) ] of
  []    -> p
  (x:_) -> error $ "unknown field " ++ show x

parse1 :: String -> Passport
parse1 = checkFields . map f . map (splitOn ":") . words where
  f [x,y] = (x,y)
  f xs = error $ "parse1: " ++ show xs

parse :: String -> [Passport]
parse = map parse1 . map unlines . splitWhen null . lines 

isvalid1 :: Passport -> Bool
isvalid1 ps = and [ elem f keys | f <- requiredFields ] where
  keys = map fst ps

--------------------------------------------------------------------------------

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
checkBYR str
  =  length str == 4 
  && all isDigit str 
  && (let n = read str :: Int in n >= 1920 && n <= 2002)

-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
checkIYR str
  =  length str == 4 
  && all isDigit str 
  && (let n = read str :: Int in n >= 2010 && n <= 2020)

-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
checkEYR str
  =  length str == 4 
  && all isDigit str 
  && (let n = read str :: Int in n >= 2020 && n <= 2030)

-- hgt (Height) - a number followed by either cm or in:
--    If cm, the number must be at least 150 and at most 193.
--    If in, the number must be at least 59 and at most 76.
checkHGT str 
  =  length aa > 0  
  && ( bb == "cm" && (n >= 150 && n <= 193) )
  || ( bb == "in" && (n >= 59  && n <= 76 ) )
  where
    (aa,bb) = span isDigit str
    n = read aa

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
checkHCL ('#':rest) = length rest == 6 && all f rest where f ch = isDigit ch || (ch >='a' && ch <='f')
checkHCL _ = False

-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
checkECL str = elem str ["amb", "blu", "brn", "gry", "grn", "hzl","oth" ]

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
checkPID str = length str == 9 && all isDigit str

isvalid2 :: Passport -> Bool
isvalid2 p = isvalid1 p && all f p where
  f (key,value) = case key of
    "byr"  -> checkBYR value
    "iyr"  -> checkIYR value
    "eyr"  -> checkEYR value
    "hgt"  -> checkHGT value
    "hcl"  -> checkHCL value
    "ecl"  -> checkECL value
    "pid"  -> checkPID value
    "cid"  -> True
    _ -> error "unknown key"

main1 = do
  text <- readFile "input4"
  let ps = parse text
  mapM_ print $ filter isvalid1 ps
  print $ length $ filter isvalid1 ps

main2 = do
  text <- readFile "input4"
  let ps = parse text
  -- mapM_ print $ filter isvalid1 ps
  print $ map isvalid2 ps
  print $ length $ filter isvalid2 ps
