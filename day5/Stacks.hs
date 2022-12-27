import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Function as F
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable (toList)

startsWithDigit :: T.Text -> Bool
startsWithDigit s
  | T.null s = False
  | otherwise = C.isDigit $ T.head s

type Stack = T.Text
type Move = [Int]

readInputData :: T.Text -> IO ([Stack], [[Int]])
readInputData filename = do
  contents <- T.lines <$> TIO.readFile (T.unpack filename)
  let (header, moves) = break (T.all C.isSpace) contents
      stacks = map (T.reverse . T.tail) $ filter startsWithDigit $ map (T.reverse . T.strip) $ T.transpose header
      toInts = map (subtract 1 . (\y -> read (T.unpack y) :: Int)) . filter startsWithDigit . T.words
      movesList = map toInts $ tail moves -- Use tail to drop the empty separator line
      movesList' = map (\x -> (head x + 1) : tail x) movesList -- Increment the move count that was previously decremented
   in return (stacks, movesList')

makeMove :: Move -> [Stack] -> [Stack]
makeMove [x, y, z] s = let rule index st
                                             | index == y = T.drop x st
                                             | index == z = T.take x (s !! y) <> st
                                             | otherwise = st
                                         in toList $ mapWithIndex rule $ fromList s

main :: IO ()
main = do
  (stacks, moves) <- readInputData (T.pack "input.txt")
  let moveFns = map makeMove moves
  let endState = foldl (\startState f -> f startState) stacks moveFns
  let result = map T.head $ filter (not . T.null) endState
  print result
