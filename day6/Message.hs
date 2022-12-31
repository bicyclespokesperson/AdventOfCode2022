import qualified Data.Sequence as S

firstFour :: String -> (S.Seq Char, [Char])
firstFour (a:b:c:d:xs) = (S.fromList [a, b, c, d], xs)

unique :: S.Seq Char -> Bool
unique _ = True --TODO: implement

--f cur (x:xs) = let next = 
main :: IO ()
main = do
  contents <- readFile "sample_input.txt"
  let (start, rest) = firstFour contents
  putStrLn "test"
