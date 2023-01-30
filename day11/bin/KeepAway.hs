{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))


--import Text.Megaparsec.String


type Parser = Parsec Void String

data Monkey = Monkey {
    items :: S.Seq Int,
    operation :: Int -> Int,
    test :: Int -> Bool,
    trueTarget :: Int,
    falseTarget :: Int
};

-- parse a single integer
int :: Parser Int
int = decimal

-- parse a list of integers separated by commas
intList :: Parser [Int]
intList = int `sepBy` string ", "

-- Parser for a single number
numberParser :: Parser Int
numberParser = read <$> some digitChar

parseMathSymbol :: Parser (Int -> Int -> Int)
parseMathSymbol = choice [
  (+) <$ char '+',
  (-) <$ char '-',
  (*) <$ char '*',
  div <$ char '/']

parseFn :: (Int -> Int -> Int) -> Parser (Int -> Int)
parseFn op = a <|> b
  where
    a = L.decimal >>= (\num -> pure (`op` num))
    b = string "old" $> (\x -> x `op` x)

parseOperation :: Parser (Int -> Int)
parseOperation = do
        _ <- string "Operation: new = old "
        op <- parseMathSymbol
        _ <- space
        parseFn op

parseEndTest :: Parser (Int -> Bool)
parseEndTest = do
        _ <- string "Test: divisible by "
        divisor <- L.decimal
        pure (\x -> mod x divisor == 0)

parseTrueBehavior :: Parser Int
parseTrueBehavior = string "If true: throw to monkey " *> L.decimal

parseFalseBehavior :: Parser Int
parseFalseBehavior = string "If false: throw to monkey " *> L.decimal

-- Parser for the input format
parseMonkey :: Parser Monkey
parseMonkey = do
  _ <- string "Monkey " *> some digitChar
  _ <- string ":" *> space *> string "Starting items: "
  items <- S.fromList <$> intList
  _ <- space
  operation <- parseOperation
  _ <- space
  test <- parseEndTest
  _ <- space
  trueTarget <- parseTrueBehavior
  _ <- space
  falseTarget <- parseFalseBehavior
  _ <- space
  return Monkey {..}

main :: IO ()
main = do
  contents <- readFile "sample_input.txt"
  case runParser (many parseMonkey) "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right r -> print (items (r !! 2))
