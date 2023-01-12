import Control.Monad.State
import qualified Data.Maybe as MB
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

cdHome = T.pack "$ cd /"
cdUp = T.pack "$ cd .."
cdPrefix = T.pack "$ cd "
lsPrefix = T.pack "$ ls"

parseLine :: T.Text -> M.Map [T.Text] Integer -> State [T.Text] (M.Map [T.Text] Integer)
parseLine cmd fileSizes
  | cdHome == cmd = do
    put [T.pack "/"]
    return fileSizes
  | cdUp == cmd = do
    modify tail -- The state is a list of directories, with the first elem being the lowest level
    return fileSizes
  | T.isPrefixOf cdPrefix cmd = do
    let dir = reverse .T.splitOn (T.pack "/") . MB.fromJust $ T.stripPrefix cdPrefix cmd
    modify (dir ++)
    return fileSizes
  | T.isPrefixOf lsPrefix cmd = do
    return fileSizes
  | T.null cmd = do
    return fileSizes
  | otherwise = do
    let (sz, name) = T.span (/= ' ') cmd
    case (readMaybe (T.unpack sz) :: Maybe Integer) of
        (Just size) -> do
            curr <- get
            let fullPath = T.strip name : curr
            return $ M.insert fullPath size fileSizes
        Nothing -> return fileSizes -- Ignore directories for now



toDirectorySizes :: M.Map [T.Text] Integer -> M.Map T.Text Integer
toDirectorySizes fileSizes = let result = M.empty
                                 --TODO: Use tails to find all the directories that should go in the map
                                 --      Might want insertWith or something as well, to automatically sum file sizes
                              in result

f :: ([[T.Text]], Integer) -> M.Map [T.Text] Integer -> M.Map [T.Text] Integer
f (dirs, sz) m = let g dir = M.insertWith (+) dir sz
                  in foldl (flip g) m dirs

part1 :: IO ()
part1 = do
  contents <- T.lines <$> TIO.readFile "input.txt"
  let (fileSizes, _) = runState (foldM (flip parseLine) M.empty contents) [T.pack "/"]
  let x1 = M.toList fileSizes
  let x2 = map (\(a, b) -> (filter (not . null) $ map tail $ filter (not . null) $ L.tails a, b)) x1
  let dirSizes = M.toList $ foldl (flip f) M.empty x2
  let result = sum . filter (<= 100000) $ map snd dirSizes
  print result

part2 :: IO ()
part2 = do
  contents <- T.lines <$> TIO.readFile "input.txt"
  let (fileSizes, _) = runState (foldM (flip parseLine) M.empty contents) [T.pack "/"]
  let x1 = M.toList fileSizes
  let x2 = map (\(a, b) -> (filter (not . null) $ map tail $ filter (not . null) $ L.tails a, b)) x1
  let dirSizes = foldl (flip f) M.empty x2
  let totalSpace = 70000000
  let neededSpace = 30000000
  let usedSpace = dirSizes M.! [T.pack "/"]
  let target = neededSpace - (totalSpace - usedSpace)
  let result = foldr1 min . filter (>= target) . map snd $ M.toList dirSizes
  print result

main :: IO ()
main = part2