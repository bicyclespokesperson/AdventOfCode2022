import qualified Data.Set as S

-- Left and Right are prelude functions, so disambiguate with *Move
data Move = UpMove | DownMove | LeftMove | RightMove

instance Show Move where
    show UpMove = "U"
    show DownMove = "D"
    show LeftMove = "L"
    show RightMove = "R"

instance Read Move where
    readsPrec _ ('U':rest) = [(UpMove, rest)]
    readsPrec _ ('D':rest) = [(DownMove, rest)]
    readsPrec _ ('L':rest) = [(LeftMove, rest)]
    readsPrec _ ('R':rest) = [(RightMove, rest)]

parseLine :: String -> [Move]
parseLine s = let [dir, count] = words s
               in replicate (read count :: Int) (read dir :: Move)

readInputData :: String -> IO [Move]
readInputData filename = concatMap parseLine . lines <$> readFile filename

tailLocation :: (Int, Int) -> (Int, Int) -> Move -> (Int, Int)
tailLocation headLoc tailLoc mv = (3, 4)

main :: IO ()
main = do
          contents <- readInputData "sample_input.txt"
          print contents
          return ()
