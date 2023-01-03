import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Foldable (toList)

firstFour :: String -> (S.Seq Char, [Char])
firstFour (a:b:c:d:xs) = (S.fromList [a, b, c, d], xs)

unique :: S.Seq Char -> Bool
unique seq = let set = Set.fromList $ toList seq
              in length seq == length set

--f cur (x:xs) idx = let next = 
f :: S.Seq Char -> [Char] -> Int -> Int
f cur (hd:rs) idx 
                | unique cur = idx
                | otherwise = case S.viewl cur of 
                    S.EmptyL -> 0
                    x S.:< xs -> f (xs S.|> hd) rs (idx + 1)
f _ [] _ = -1


main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (start, rest) = firstFour contents
  let idx = f start rest 4
  print idx
