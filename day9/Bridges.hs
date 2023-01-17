
-- Left and Right are prelude functions, so disambiguate with *Move
data Move = UpMove | DownMove | LeftMove | RightMove

toMove :: String -> Move
toMove "U" = UpMove
toMove "D" = DownMove
toMove "L" = LeftMove
toMove "R" = RightMove

parseLine :: String -> [Move]
parseLine s = let [dir, count] = words s
               in replicate (read count :: Int) (toMove dir)

readInputData :: String -> IO [Move]
readInputData filename = do
                            contents <- lines <$> readFile filename
                            return $ concatMap parseLine contents

tailLocation :: (Int, Int) -> (Int, Int) -> Move -> (Int, Int)
tailLocation headLoc tailLoc mv = (3, 4)

main :: IO ()
main = print "test"
