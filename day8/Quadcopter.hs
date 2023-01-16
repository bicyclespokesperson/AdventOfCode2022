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
      loop cc dist =
        case M.lookup cc mp of
          (Just h) -> h < height && loop (dirFn cc) (dist + 1)
          Nothing -> True
   in loop (dirFn coord) 0

part1 :: IO ()
part1 = do
  grid <- readInputData "input.txt"
  let findEdgeAllDirs = map (findEdge grid) [left, right, up, down]
  let isVisible coord = any ($ coord) findEdgeAllDirs
  let res = M.foldrWithKey (\coord _ count -> count + if isVisible coord then 1 else 0) 0 grid
  print res

main :: IO ()
main = part1