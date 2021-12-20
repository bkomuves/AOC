
{-# LANGUAGE PackageImports #-}

import Data.Char
import "parsec1" Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------

data Expr 
  = Add Expr Expr
  | Mul Expr Expr
  | Lit Integer
  deriving Show

eval :: Expr -> Integer
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Lit n) = n

--------------------------------------------------------------------------------
-- part 1

parseLineV1 :: String -> Expr
parseLineV1 str = 

  case runParser lineP () "<input>" (filter (not . isSpace) str) of
    Left msg -> error (show msg)
    Right e  -> e

  where

    lineP :: Parser Expr
    lineP = do
      e <- exprP
      eof
      return e
    
    exprP :: Parser Expr
    exprP = opsP
    
    atomP :: Parser Expr
    atomP = parenP <|> numP
    
    numP :: Parser Expr
    numP = do
      xs <- many1 digit
      return (Lit $ read xs)
    
    parenP :: Parser Expr
    parenP = do
      char '('
      e <- exprP
      char ')'
      return e
    
    opsP :: Parser Expr
    opsP = do
      a <- atomP
      obs <- many $ do
        op  <- oneOf "+*"
        b   <- atomP
        return (op,b)
      let f e1 (o,e2) = case o of 
            '+' -> Add e1 e2
            '*' -> Mul e1 e2
      return $ foldl f a obs
  
--------------------------------------------------------------------------------
-- part 2

parseLineV2 :: String -> Expr
parseLineV2 str = 

  case runParser lineP () "<input>" (filter (not . isSpace) str) of
    Left msg -> error (show msg)
    Right e  -> e

  where

    lineP :: Parser Expr
    lineP = do
      e <- exprP
      eof
      return e
    
    exprP :: Parser Expr
    exprP = mulP
    
    atomP :: Parser Expr
    atomP = parenP <|> numP
    
    numP :: Parser Expr
    numP = do
      xs <- many1 digit
      return (Lit $ read xs)
    
    parenP :: Parser Expr
    parenP = do
      char '('
      e <- exprP
      char ')'
      return e
    
    addP :: Parser Expr
    addP = do
      e1 <- atomP
      es <- many $ do
        char '+'
        atomP
      return $ foldl Add e1 es
    
    mulP :: Parser Expr
    mulP = do
      e1 <- addP
      es <- many $ do
        char '*'
        addP
      return $ foldl Mul e1 es

--------------------------------------------------------------------------------

part1 :: IO ()
part1 = do
  putStrLn "\npart 1"
  ls <- lines <$> readFile "input18"
  let es = map parseLineV1 ls
  -- mapM_ print es
  let ns = map eval es
  -- putStrLn $ "results   = " ++ show ns
  putStrLn $ "final sum = " ++ show (sum ns)

part2 :: IO ()
part2 = do
  putStrLn "\npart 2"
  ls <- lines <$> readFile "input18"
  let es = map parseLineV2 ls
  -- mapM_ print es
  let ns = map eval es
  -- putStrLn $ "results   = " ++ show ns
  putStrLn $ "final sum = " ++ show (sum ns)

main :: IO ()
main = do
  part1
  part2

