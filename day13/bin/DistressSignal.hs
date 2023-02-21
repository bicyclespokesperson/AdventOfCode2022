module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (ord)
import qualified Data.Array as A
import Data.Void (Void)
import qualified Data.Set as S
import Control.Monad (guard)
import Data.Maybe (fromJust)


part1 :: IO ()
part1 = do
  contents <- readFile "sample_input.txt"
  print $ head contents

part2 :: IO ()
part2 = do
  contents <- readFile "sample_input.txt"
  print $ head contents

main :: IO ()
main = part2
