module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (ord)
import qualified Data.Array as A
import Data.Void (Void)
import qualified Data.Set as S
import qualified Control.Monad.State as MS
import Control.Monad (foldM, guard, (<=<))
import Data.Maybe (fromJust)
import Data.List (foldl1')
import qualified Control.Monad.Loops as CML

type Coord2 = (Int, Int)
type Grid2D a = A.Array Coord2 a
type Parser = Parsec Void String

charToHeight :: Char -> Int
charToHeight 'S' = 100
charToHeight 'E' = charToHeight 'z' + 1
charToHeight c = ord c - ord 'a'

parse2DDigitArray :: Parser (Grid2D Int)
parse2DDigitArray = digitsToArray <$> sepEndBy1 parseDigitLine eol

digitsToArray :: [[Int]] -> Grid2D Int
digitsToArray inputs = A.listArray ((0, 0), (length inputs - 1, length (head inputs) - 1)) (concat inputs)

-- (fmap charToHeight) :: [Char] -> [Int], and that function is applied to the value in the parser's monadic context. Could use map instead of fmap
parseDigitLine :: Parser [Int]
parseDigitLine = fmap charToHeight <$> some letterChar

neighbors :: Grid2D Int -> Coord2 -> [Coord2]
neighbors grid (x, y) = do 
                            let ((xmin, ymin), (xmax, ymax)) = A.bounds grid
                            (x', y') <- [(x+1, y), (x-1,y), (x, y+1), (x, y-1), (x, y)]
                            guard (x' `elem` [xmin..xmax] && y' `elem` [ymin..ymax] && ((grid A.! (x', y')) - (grid A.! (x, y))) <= 1)
                            return (x', y')

singleStep :: Grid2D Int -> S.Set Coord2 -> S.Set Coord2
singleStep grid =  let neighbors' = neighbors grid
                    in S.foldl' (\a b -> S.union a (S.fromList $ neighbors' b)) S.empty

-- Potential problem: If a square is reachable from two current squares, it will be 
-- present twice in the resulting list. I could, potentially, create a new function 
-- visit all neighbors, updating the state at each iteration. That would defeat the 
-- entire purpose of using the list monad though.
-- Also, we only want, really, the new visited nodes present at the end of this?
{-
neighbors' :: Coord2 -> Coord2 -> Grid2D Int -> Coord2 -> MS.State (S.Set Coord2) [Coord2]
neighbors' (xmin, ymin) (xmax, ymax) grid (x, y) = do
    visited <- MS.get
    let newNeighbors' = [(x+1, y), (x-1,y), (x, y+1), (x, y-1)] >>= (\(x', y') -> guard (x' `elem` [xmin..xmax] 
                                                                                         && y' `elem` [ymin..ymax] 
                                                                                         && (((grid A.! (x', y')) - (grid A.! (x, y))) <= 1) 
                                                                                         && (x', y') `S.notMember` visited) >> return (x', y'))
    MS.put (S.union visited (S.fromList newNeighbors'))
    return newNeighbors'
-}


{-
fewestSteps :: Grid2D Int -> Coord2 -> Coord2 -> Int
fewestSteps grid start end = let (min', max') = A.bounds grid
                                 f = neighbors min' max' grid
                                 t = iterate (>>= f) [start]
                              in length $ takeWhile (notElem end) t

fewestSteps' :: Grid2D Int -> Coord2 -> Coord2 -> [[Coord2]]
fewestSteps' grid start end = let (min', max') = A.bounds grid
                                  f = neighbors' min' max' grid
                                  stepFn :: [Coord2] -> MS.State (S.Set Coord2) [Coord2]
                                  stepFn cur = let newNeighbors = mapM f cur in concat <$> newNeighbors
                                  states = CML.iterateUntilM (\xs -> True) stepFn [start]
                                  (curr, _) = MS.runState states S.empty
                                in [curr]
                                -}

find :: Grid2D Int -> Int -> Maybe Coord2
find grid val = case filter (\(_, e) -> val == e) (A.assocs grid) of
                  ((i, _):_) -> Just i
                  [] -> Nothing

printHelper :: [[Coord2]] -> IO [()]
printHelper = traverse print

-- https://stackoverflow.com/questions/25780555/best-way-to-generate-a-list-with-state-haskell
-- https://cloudnative.ly/haskell-mapping-with-state-7a07e3c2cbf9

--x = sequenceA

fewestSteps :: Grid2D Int -> Coord2 -> Coord2 -> Int --[S.Set Coord2]
fewestSteps grid start end = let g = singleStep grid
                                 vals = iterate g $ S.fromList [start]
                              --in vals
                              in length $ takeWhile (not . S.member end) vals

part1 :: IO ()
part1 = do
  contents <- readFile "sample_input.txt"
  case runParser parse2DDigitArray "filename_for_error_message.txt" contents of
      Left e -> putStr (errorBundlePretty e)
      Right g -> do
                      let start = fromJust $ find g 100
                      let end = fromJust $ find g $ charToHeight 'z' + 1
                      print start
                      print end
                      let a = fewestSteps g start end
                      print a
                      return ()

main :: IO ()
main = part1