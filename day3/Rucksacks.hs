import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text.IO
import Data.Char (ord, isLower, isUpper, toUpper, toLower)
import Data.Maybe (mapMaybe)


readInputData :: String -> IO [(T.Text, T.Text)]
readInputData filename = do 
    contents <- T.lines <$> Data.Text.IO.readFile filename
    let pairs = map (\txt -> let sz = T.length txt in T.splitAt (sz `div` 2) txt) contents
    return pairs

findMatchingChar :: (T.Text, T.Text) -> Maybe Char
findMatchingChar (a, b) = let s = S.fromList $ T.unpack a
                          in L.find (`S.member` s) (T.unpack b)

priority :: Char -> Int
priority c 
        | isLower c = 1 + (ord c - ord 'a')
        | isUpper c = 26 + priority (toLower c)

main :: IO()
main = do
    xs <- readInputData "input.txt"
    let priorities = mapMaybe (fmap priority . findMatchingChar) xs
    print $ sum priorities


