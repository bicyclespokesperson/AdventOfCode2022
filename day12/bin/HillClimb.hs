module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (ord)
import qualified Data.Array as A
import Data.Void (Void)
import Control.Monad (guard, (<=<))
import Data.Maybe (fromJust)

type Coord2 = (Int, Int)
type Grid2D a = A.Array Coord2 a

type Parser = Parsec Void String

charToHeight :: Char -> Int
charToHeight 'S' = 100
charToHeight 'E' = ord 'z' + 1
charToHeight c = ord c - ord 'a'

parse2DDigitArray :: Parser (Grid2D Int)
parse2DDigitArray = digitsToArray <$> sepEndBy1 parseDigitLine eol

digitsToArray :: [[Int]] -> Grid2D Int
digitsToArray inputs = A.listArray ((0, 0), (length inputs - 1, length (head inputs) - 1)) (concat inputs)

-- (fmap charToHeight) :: [Char] -> [Int], and that function is applied to the value in the parser's monadic context. Could use map instead of fmap
parseDigitLine :: Parser [Int]
parseDigitLine = fmap charToHeight <$> some letterChar

neighbors :: Coord2 -> Coord2 -> Grid2D Int -> Coord2 -> [Coord2]
neighbors (xmin, ymin) (xmax, ymax) grid (x, y) = do 
                                                (x', y') <- [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]
                                                guard (x' `elem` [xmin..xmax] && y' `elem` [ymin..ymax] && ((grid A.! (x', y')) - (grid A.! (x, y))) <= 1)
                                                return (x', y')

fewestSteps :: Grid2D Int -> Coord2 -> Coord2 -> Int
fewestSteps grid start end = let (min', max') = A.bounds grid
                                 f = neighbors min' max' grid
                                 t = iterate (>>= f) [start]
                              in length $ takeWhile (notElem end) t

fewestSteps' :: Grid2D Int -> Coord2 -> Coord2 -> [[Coord2]]
fewestSteps' grid start end = let (min', max') = A.bounds grid
                                  f = neighbors min' max' grid
                                  t = iterate (>>= f) [start]
                               in take 5 t

find :: Grid2D Int -> Int -> Maybe Coord2
find grid val = case filter (\(_, e) -> val == e) (A.assocs grid) of
                  ((i, _):_) -> Just i
                  [] -> Nothing

printHelper :: [[Coord2]] -> IO [()]
printHelper = traverse print

--x = sequenceA

main :: IO ()
main = do
  contents <- readFile "sample_input.txt"
  case runParser parse2DDigitArray "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right grid -> do
                      let start = fromJust $ find grid 100
                      let end = fromJust $ find grid $ ord 'z' + 1
                      print start
                      print end
                      _ <- printHelper $ fewestSteps' grid start end
                      return ()
                      --print $ fewestSteps grid start end


-- list applicatives & BFS, similar to knights tour
