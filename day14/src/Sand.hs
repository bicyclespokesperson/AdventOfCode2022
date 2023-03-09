module Main where

--See https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs for inspiration

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as LL (decimal)

type Parser = Parsec Void String

readCoordinatePair :: Parser (Int, Int)
readCoordinatePair = do
                    x <- LL.decimal
                    _ <- string ","
                    y <- LL.decimal
                    return (x, y)

readRocks :: Parser [[(Int, Int)]]
readRocks = readRock `sepBy` newline
              where readRock = readCoordinatePair `sepBy` string " -> "

part1 :: IO ()
part1 = do
  contents <- readFile "sample_input.txt"
  case runParser readRocks "filename_for_error_message.txt" contents of
      Left err -> putStr (errorBundlePretty err)
      Right rocks -> do
                      print $ tail rocks


part2 :: IO ()
part2 = do
  contents <- readFile "sample_input.txt"
  print $ head contents

main :: IO ()
main = part1
