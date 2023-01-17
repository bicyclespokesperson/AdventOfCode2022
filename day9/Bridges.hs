import Data.Foldable (foldl')
import qualified Data.Set as S

-- Left and Right are prelude functions, so disambiguate with *Move
data Move
  = UpMove
  | DownMove
  | LeftMove
  | RightMove

instance Show Move where
  show :: Move -> String
  show UpMove = "U"
  show DownMove = "D"
  show LeftMove = "L"
  show RightMove = "R"

instance Read Move where
  readsPrec :: Int -> String -> [(Move, String)]
  readsPrec _ ('U':rest) = [(UpMove, rest)]
  readsPrec _ ('D':rest) = [(DownMove, rest)]
  readsPrec _ ('L':rest) = [(LeftMove, rest)]
  readsPrec _ ('R':rest) = [(RightMove, rest)]

makeMove :: (Int, Int) -> Move -> (Int, Int)
makeMove (x, y) UpMove = (x, y + 1)
makeMove (x, y) DownMove = (x, y - 1)
makeMove (x, y) LeftMove = (x - 1, y)
makeMove (x, y) RightMove = (x + 1, y)

parseLine :: String -> [Move]
parseLine s =
  let [dir, count] = words s
   in replicate (read count :: Int) (read dir :: Move)

readInputData :: String -> IO [Move]
readInputData filename = concatMap parseLine . lines <$> readFile filename

-- (x,y) is head location, (a,b) is tail location
tailLocation :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailLocation (x, y) (a, b)
  | abs (x - a) <= 1 && abs (y - b) <= 1 = (a, b)
  | (x - a == -2) && (y == b) = (a - 1, b)
  | (x - a == 2) && (y == b) = (a + 1, b)
  | (y - b == -2) && (x == a) = (a, b - 1)
  | (y - b == 2) && (x == a) = (a, b + 1)
  | x < a && y < b = (a - 1, b - 1)
  | x < a && y > b = (a - 1, b + 1)
  | x > a && y < b = (a + 1, b - 1)
  | x > a && y > b = (a + 1, b + 1)

tailLocations :: [(Int, Int)] -> [(Int, Int)]
tailLocations =
  reverse .
  foldl' (\acc headLoc -> tailLocation headLoc (head acc) : acc) [(0, 0)]

main :: IO ()
main = do
  headMoves <- readInputData "input.txt"
  let headLocations =
        reverse $
        foldl'
          (\coords mv -> makeMove (head coords) mv : coords)
          [(0, 0)]
          headMoves
  let allTailLocations = iterate tailLocations headLocations !! 9 -- Set this to 1 for part 1
  print . S.size $ S.fromList allTailLocations
