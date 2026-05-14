-- Parsing RegExp
--
-- Symblos are 1 char (a, b, 1, 0, ..)
-- Operations: +, *, () where
-- + is a union
-- * is a Kleene star
-- () just a brackets

module Parser where

import Text.Parsec
    ( ParseError,
      try,
      char,
      satisfy,
      spaces,
      between,
      many1,
      (<|>),
      many,
      parse )
import Text.Parsec.String (Parser)
import Data.Char (isAlphaNum)

data Regex
  = Sym Char           -- a, b, 1, 0...
  | Cat Regex Regex    -- AB
  | Cup Regex Regex    -- A+B
  | Star Regex         -- A*
  deriving (Show, Eq)

-- Parse entire expression
parseExpr :: Parser Regex
parseExpr = do
  spaces
  first <- parseTerm
  rest <- many (spaces >> char '+' >> spaces >> parseTerm)
  return $ foldl Cup first rest

-- Parse one term
parseTerm :: Parser Regex
parseTerm = do
  factors <- many1 parseFactor
  return $ foldl1 Cat factors

-- Parse one factor (symbol, brackets or Kleene star)
parseFactor :: Parser Regex
parseFactor = try (do
  atom <- parseAtom
  stars <- many (char '*')
  return $ foldr (const Star) atom stars)

-- Parse one atom (symbol or expr in brackets)
parseAtom :: Parser Regex
parseAtom = 
      try (Sym <$> parseSymbol)
  <|> between (char '(') (char ')') parseExpr

-- Parse one symbol
parseSymbol :: Parser Char
parseSymbol = satisfy (\c -> isAlphaNum c && c `notElem` "()+*")

-- Parse regexp (or return an error)
parseRegex :: String -> Either ParseError Regex
parseRegex = parse parseExpr ""

-- Print parsed Regexp
regexToString :: Regex -> String
regexToString (Sym c) = [c]
regexToString (Cat a b) = "(" ++ regexToString a ++ regexToString b ++ ")"
regexToString (Cup a b) = "(" ++ regexToString a ++ "+" ++ regexToString b ++ ")"
regexToString (Star r) = "(" ++ regexToString r ++ ")*"