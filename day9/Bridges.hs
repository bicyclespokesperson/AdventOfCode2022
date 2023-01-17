import qualified Data.Set as S

-- Left and Right are prelude functions, so disambiguate with *Move
data Move = UpMove | DownMove | LeftMove | RightMove

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
makeMove (x, y) UpMove = (x, y+1)
makeMove (x, y) DownMove = (x, y-1)
makeMove (x, y) LeftMove = (x-1, y)
makeMove (x, y) RightMove = (x+1, y)


parseLine :: String -> [Move]
parseLine s = let [dir, count] = words s
               in replicate (read count :: Int) (read dir :: Move)

readInputData :: String -> IO [Move]
readInputData filename = concatMap parseLine . lines <$> readFile filename

tailLocation :: (Int, Int) -> (Int, Int) -> Move -> (Int, Int)
tailLocation headLoc tailLoc mv = (3, 4)

main :: IO ()
main = do
          headMoves <- readInputData "sample_input.txt"
          let headLocations = reverse $ foldr (\mv coords -> makeMove (head coords) mv : coords) [(0, 0)] (reverse headMoves)
          print headMoves
          print headLocations
