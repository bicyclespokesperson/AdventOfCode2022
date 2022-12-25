import Control.Monad.Cont (cont)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import qualified Data.List as L

split :: String -> Char -> [String]
split [] _ = []
split s ch =
  let (prefix, rest) = break (== ch) s
   in case rest of
        [] -> prefix : split rest ch
        x:xs -> prefix : split (tail rest) ch

readInputData :: String -> IO [(Range, Range)]
readInputData filename = do
  contents <- lines <$> readFile filename
  let toTuple lineLs = (head lineLs, head (tail lineLs))
  let x = map (toTuple . (`split` ',')) contents
  return (map (bimap toRange toRange) x)

data Range =
  Range
    { start :: Int
    , end :: Int
    }
  deriving (Show, Eq)

contains :: Range -> Range -> Bool
contains r1 r2 = start r1 <= start r2 && end r1 >= end r2

inRange :: (Int, Int) -> Int -> Bool
inRange (low, high) val = val >= low && val <= high

hasOverlap :: Range -> Range -> Bool
hasOverlap r1 r2 =
  let rg = (start r1, end r1)
   in inRange rg (start r2) || inRange rg (end r2)

-- Input is of the form: 18-300
toRange :: String -> Range
toRange s =
  let (startStr, rest) = span isDigit s
      (_, endStr) = break isDigit rest
   in Range (read startStr :: Int) (read endStr :: Int)

main :: IO ()
main = do
  contents <- readInputData "input.txt"
  let contained =
        filter (\(r1, r2) -> hasOverlap r1 r2 || hasOverlap r2 r1) contents
  print $ length contained
