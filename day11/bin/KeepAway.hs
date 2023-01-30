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

type Parser = Parsec Void String

data Monkey = Monkey {
    items :: S.Seq Int,
    operation :: Int -> Int,
    test :: Int -> Bool,
    trueTarget :: Int,
    falseTarget :: Int
};

-- parse a list of integers separated by commas
parseIntList :: Parser [Int]
parseIntList = decimal `sepBy` string ", "

parseMathSymbol :: Parser (Int -> Int -> Int)
parseMathSymbol = choice [
  (+) <$ char '+',
  (-) <$ char '-',
  (*) <$ char '*',
  div <$ char '/']

parseOperation :: Parser (Int -> Int)
parseOperation = do
        _ <- string "Operation: new = old "
        op <- parseMathSymbol
        _ <- space
        (L.decimal >>= (\num -> pure (`op` num))) <|> 
          (string "old" $> (\x -> x `op` x))

parseEndTest :: Parser (Int -> Bool)
parseEndTest = do
        _ <- string "Test: divisible by "
        divisor <- L.decimal
        pure (\x -> mod x divisor == 0)

parseMonkey :: Parser Monkey
parseMonkey = do
  _ <- string "Monkey " *> some digitChar
  _ <- string ":" *> space *> string "Starting items: "
  items <- S.fromList <$> parseIntList
  _ <- space
  operation <- parseOperation
  _ <- space
  test <- parseEndTest
  _ <- space
  trueTarget <- string "If true: throw to monkey " *> L.decimal
  _ <- space
  falseTarget <- string "If false: throw to monkey " *> L.decimal
  _ <- space
  return Monkey {..}

main :: IO ()
main = do
  contents <- readFile "sample_input.txt"
  case runParser (many parseMonkey) "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right r -> print (items (r !! 2))
