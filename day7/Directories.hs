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
    put []
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

main :: IO ()
main = do
  contents <- T.lines <$> TIO.readFile "sample_input.txt"
  let f = flip parseLine
  let (fileSizes, _) = runState (foldM f (M.empty :: M.Map [T.Text] Integer) contents) []
  print fileSizes
