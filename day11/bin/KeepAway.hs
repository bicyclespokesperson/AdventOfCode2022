{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L

--import Text.Megaparsec.String


type Parser = Parsec Void String

data Monkey = Monkey {
    items :: S.Seq Int,
    operation :: Int -> Int,
    test :: Int -> Bool
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


-- TODO: Support old * old
parseOperation :: Parser (Int -> Int)
parseOperation = do
        _ <- string "Operation: new = old "
        op <- parseMathSymbol
        _ <- space
        num <- L.decimal
        pure (`op` num)

parseEndTest :: Parser (Int -> Bool)
parseEndTest = do
        _ <- string "Test: divisible by "
        divisor <- L.decimal
        pure (\x -> mod x divisor == 0)
        

-- Parser for the input format
parseInput :: Parser Monkey
parseInput = do
  _ <- string "Monkey " *> some digitChar
  _ <- string ":" *> newline *> space *> string "Starting items: "
  items <- S.fromList <$> intList
  _ <- newline *> space
  operation <- parseOperation
  _ <- newline *> space
  test <- parseEndTest
  _ <- newline
  --TODO: True and False behavior
  return Monkey {..}

-- Example usage
exampleInput :: String
exampleInput = "Monkey 0:\n  Starting items: 79, 98"

main :: IO ()
main = do
  contents <- readFile "sample_input.txt"
  case runParser parseInput "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right r -> print (items r)
