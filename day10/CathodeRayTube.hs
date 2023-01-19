
parseLine :: String -> [Int -> Int]
parseLine "noop" = [id]
parseLine addX = [id, (+) (read x :: Int)] where
    [_, x] = words addX

readInputData :: String -> IO [Int -> Int]
readInputData filename = concatMap parseLine . lines <$> readFile filename

analyzeResults :: [Int] -> Int
analyzeResults xs = sum . map (uncurry (*)) $ filter (\(i, x) -> (i - 20) `mod` 40 == 0) $ zip [1..] xs

main :: IO ()
main = do
        fs <- readInputData "sample_input_2.txt"
        let res = scanl (\v f -> f v) 1 fs
        print $ analyzeResults res
