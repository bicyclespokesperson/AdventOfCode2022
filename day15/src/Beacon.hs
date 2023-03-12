module Main where

--See https://github.com/clatisus/advent-of-code-y2022/blob/master/src/Day13.hs for inspiration

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as LL (decimal)

type Parser = Parsec Void String


part1 :: IO ()
part1 = do
          contents <- readFile "sample_input.txt"
          print . head $ lines contents

part2 :: IO ()
part2 = print "part2"

main :: IO ()
main = part1
