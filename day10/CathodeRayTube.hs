import Data.List (splitAt, intercalate)
import Control.Applicative (ZipList(ZipList, getZipList))

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

parseLine :: String -> [Int -> Int]
parseLine "noop" = [id]
parseLine addX = [id, (+) (read x :: Int)]
  where
    [_, x] = words addX

readInputData :: String -> IO [Int -> Int]
readInputData filename = concatMap parseLine . lines <$> readFile filename

-- Is this mostly seeing when i and x are equal in the xs array? Then regenerating i starting from 0 and 2?
-- join is intercalate
analyzePart1 :: [Int] -> Int
analyzePart1 xs =
  sum . map (uncurry (*)) $
  filter (\(i, x) -> (i - 20) `mod` 40 == 0) $ zip [1 ..] xs

part1 :: IO ()
part1 = do
  fs <- readInputData "sample_input_2.txt"
  let res = scanl (\v f -> f v) 1 fs
  print $ analyzePart1 res

renderGrid :: [(Int, Int)] -> String
renderGrid xs = intercalate "\n" grid
                 where grid = splitEvery 40 $ map (\(a, b) -> if a == b then '#' else '.') xs

main :: IO ()
main = do
  fs <- readInputData "sample_input_2.txt"
  let res = scanl (\v f -> f v) 1 fs
  let indices = concat $ replicate 6 [1..40]
  let indices2 = tail indices ++ [0]
  let indices3 = 0 : init indices
  let combineStrs x y z = if x == '#' || y == '#' then '#' else y
  let a1 = renderGrid $ zip indices res
  let a2 = renderGrid $ zip indices2 res
  let a3 = renderGrid $ zip indices3 res
  let res = combineStrs <$> ZipList a1 <*> ZipList a2 <*> ZipList a3
  putStrLn $ getZipList res
