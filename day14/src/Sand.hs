module Main where

--See https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs for inspiration

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as LL (decimal)

type Parser = Parsec Void String

type Grid = S.Set (Int, Int)

readCoordinatePair :: Parser (Int, Int)
readCoordinatePair = do
                    x <- LL.decimal
                    _ <- string ","
                    y <- LL.decimal
                    return (x, y)

readRocks :: Parser [[(Int, Int)]]
readRocks = readRock `sepBy` newline
              where readRock = readCoordinatePair `sepBy` string " -> "

--addSegment :: Grid -> [(Int, Int)] -> Grid
--addSegment

addRock :: Grid -> [(Int, Int)] -> Grid
addRock grid [] = grid
addRock grid [_] = grid
addRock grid ((x1, y1):(x2, y2):xs) = let indicesToSet = if x1 == x2 then [(x1, y) | y <- [y1..y2]] else [(x, y1) | x <- [x1..x2]]
                                          newGrid = foldl (flip S.insert) grid indicesToSet
                                       in addRock newGrid ((x2, y2):xs)

part1 :: IO ()
part1 = do
  contents <- readFile "sample_input.txt"
  case runParser readRocks "filename_for_error_message.txt" contents of
      Left err -> putStr (errorBundlePretty err)
      Right rocks -> do
                      let grid = foldl addRock S.empty rocks
                      let charForCoord coord = if S.member coord grid then 'o' else '.'
                      putStrLn $ unlines [ [ charForCoord (x, y) | x <- [1..100] ] | y <- [1..100] ]
                      print $ tail rocks

part2 :: IO ()
part2 = do
  contents <- readFile "sample_input.txt"
  print $ head contents

{-
charForCoord :: (Int, Int) -> Char
charForCoord (x, y)
    | x == y = '\\'
    | x + y == 101 = '/'
    | x `mod` 10 == 0 || y `mod` 10 == 0 = '+'
    | otherwise = '-'
    -}

main :: IO ()
main = part1
