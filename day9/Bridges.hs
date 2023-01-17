
-- Left and Right are prelude functions, so disambiguate with *Move
data Move = UpMove | DownMove | LeftMove | RightMove deriving Show

toMove :: String -> Move
toMove "U" = UpMove
toMove "D" = DownMove
toMove "L" = LeftMove
toMove "R" = RightMove

parseLine :: String -> [Move]
parseLine s = let [dir, count] = words s
               in replicate (read count :: Int) (toMove dir)

readInputData :: String -> IO [Move]
readInputData filename = concatMap parseLine . lines <$> readFile filename

tailLocation :: (Int, Int) -> (Int, Int) -> Move -> (Int, Int)
tailLocation headLoc tailLoc mv = (3, 4)

main :: IO ()
main = do
          contents <- readInputData "sample_input.txt"
          print contents
          return ()
