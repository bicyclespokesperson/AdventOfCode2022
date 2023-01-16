import Data.Char (digitToInt)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Sequence as S

toCoords :: Int -> S.Seq Char -> S.Seq ((Int, Int), Int)
toCoords i = S.mapWithIndex (\j ch -> ((i, j), digitToInt ch))

readInputData :: String -> IO (M.Map (Int, Int) Int)
readInputData filename = do
  contents <- fmap S.fromList . S.fromList . lines <$> readFile filename
  let grid = S.mapWithIndex toCoords contents
  return $ foldr1 M.union $ M.fromList . toList <$> grid

up :: (Int, Int) -> (Int, Int)
up (i, j) = (i - 1, j)

down :: (Int, Int) -> (Int, Int)
down (i, j) = (i + 1, j)

left :: (Int, Int) -> (Int, Int)
left (i, j) = (i, j - 1)

right :: (Int, Int) -> (Int, Int)
right (i, j) = (i, j + 1)

findEdge ::
     M.Map (Int, Int) Int -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Bool
findEdge mp dirFn coord =
  let height = mp M.! coord
      f cc dist =
        case M.lookup cc mp of
          (Just h) -> h < height && f (dirFn cc) (dist + 1)
          Nothing -> True
   in f (dirFn coord) 0

main :: IO ()
main = do
  grid <- readInputData "sample_input.txt"
  let coord = (1, 3)
  print $ grid M.! up coord
  print $ findEdge grid up coord
