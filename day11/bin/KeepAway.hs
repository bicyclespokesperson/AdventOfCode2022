{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void (Void)
import Data.List (sort)
import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))
import Control.Lens ((.~), element)

type Parser = Parsec Void String

data Monkey = Monkey {
    items :: S.Seq Int,
    operation :: Int -> Int,
    test :: Int -> Bool,
    trueTarget :: Int,
    falseTarget :: Int,
    totalSeen :: Int
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
  let totalSeen = 0 -- length items
  return Monkey {..}

pushItem :: Int -> Monkey -> Monkey
pushItem val m = let newItems = (items m S.|> val) in m {items = newItems} 

popItem :: Monkey -> (Monkey, Int)
popItem m = case S.viewl (items m) of
              S.EmptyL -> error "popItem: Empty sequence"
              (start S.:< rest) -> (m {items = rest, totalSeen = totalSeen m + 1}, start)

switchItem :: Int -> Int -> Int -> [Monkey] -> [Monkey]
switchItem from to new ms = let (m, _) = popItem $ ms !! from 
                         in (element from .~ m) $ (element to .~ pushItem new (ms !! to)) ms

boredomAdjustment :: Int -> Int
boredomAdjustment itm = itm `div` 3

inspectItem :: Int -> [Monkey] -> [Monkey]
inspectItem i ms = let monkey = (ms !! i)
                       itm = S.index (items monkey) 0
                       moreWorried = operation monkey itm
                       lessWorried = boredomAdjustment moreWorried
                       target = (if test monkey lessWorried then trueTarget else falseTarget) monkey
                    in switchItem i target lessWorried ms

runRound' :: Int -> [Monkey] -> [Monkey]
runRound' i ms
  | i >= length ms = ms
  | null (items $ ms !! i) = runRound' (i + 1) ms
  | otherwise = runRound' i $ inspectItem i ms

runRound :: [Monkey] -> [Monkey]
runRound = runRound' 0

calculateViews :: [Monkey] -> [Int]
calculateViews ms = let states = take 21 $ iterate runRound ms
                     in map totalSeen $ last states

main :: IO ()
main = do
  contents <- readFile "input.txt"
  case runParser (many parseMonkey) "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right monkeys -> do print $ map items monkeys
                          print . map items $ runRound monkeys
                          print $ product . take 2 . reverse . sort $ calculateViews monkeys
