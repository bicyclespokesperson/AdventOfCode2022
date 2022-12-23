import qualified Data.List as L
import System.IO


data Shape = Rock | Paper | Scissors
    deriving (Bounded, Eq, Enum, Show)

data Outcome = Win | Draw | Loss
    deriving Show

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred c
        | c == minBound = maxBound
        | otherwise = pred c

    csucc :: a -> a
    csucc c
        | c == maxBound = minBound
        | otherwise     = succ c

instance CyclicEnum Shape

-- This could be read?
toShape :: Char -> Shape
toShape c | c == 'A' || c == 'X' = Rock
          | c == 'B' || c == 'Y' = Paper
          | c == 'C' || c == 'Z' = Scissors

battle :: Shape -> Shape -> Outcome
battle playerShape oppShape 
    | playerShape == oppShape = Draw
    | cpred playerShape == oppShape = Win
    | otherwise = Loss

scoreSh :: Shape -> Int
scoreSh Rock = 1
scoreSh Paper = 2
scoreSh Scissors = 3

scoreOt :: Outcome -> Int
scoreOt Loss = 0
scoreOt Draw = 3
scoreOt Win = 6

score :: Shape -> Outcome -> Int
score shp result = scoreSh shp + scoreOt result

lineToShapes :: String -> (Shape, Shape)
lineToShapes l = (toShape (head l), toShape (head (dropWhile (== ' ') (tail l))))

readInputData :: String -> IO [(Shape, Shape)]
readInputData filename = do
    moveList <- lines <$> readFile filename
    return (map lineToShapes moveList)

main :: IO ()
main = do
    battles <- readInputData "input.txt"
    print $ sum $ map (\(oppChoice, plChoice) -> score plChoice (battle plChoice oppChoice)) battles