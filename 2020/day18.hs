
{-# LANGUAGE PackageImports #-}

import Data.Char
import "parsec1" Text.ParserCombinators.Parsec

data Expr 
  = Add Expr Expr
  | Mul Expr Expr
  | Lit Integer
  deriving Show

eval :: Expr -> Integer
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Lit n) = n

parseLine :: String -> Expr
parseLine str = case runParser lineP () "<input>" (filter (not . isSpace) str) of
  Left msg -> error (show msg)
  Right e  -> e

--------------------------------------------------------------------------------
-- * Part 1

{- 

lineP = do
  e <- exprP
  eof
  return e

exprP = opsP

atomP = parenP <|> numP

numP = do
  xs <- many1 digit
  return (Lit $ read xs)

parenP = do
  char '('
  e <- exprP
  char ')'
  return e

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

-}

--------------------------------------------------------------------------------

lineP = do
  e <- exprP
  eof
  return e

exprP = mulP

atomP = parenP <|> numP

numP = do
  xs <- many1 digit
  return (Lit $ read xs)

parenP = do
  char '('
  e <- exprP
  char ')'
  return e

addP = do
  e1 <- atomP
  es <- many $ do
    char '+'
    atomP
  return $ foldl Add e1 es

mulP = do
  e1 <- addP
  es <- many $ do
    char '*'
    addP
  return $ foldl Mul e1 es

--------------------------------------------------------------------------------

main = do
  -- ls <- lines <$> readFile "test18b"
  ls <- lines <$> readFile "input18"
  let es = map parseLine ls
  mapM_ print es
  let ns = map eval es
  print ns
  print $ sum ns

