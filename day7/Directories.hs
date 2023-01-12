import Control.Monad.State
import qualified Data.Maybe as MB
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

cdHome = T.pack "$ cd /"
cdPrefix = T.pack "$ cd"
lsPrefix = T.pack "$ ls"

parseLine :: T.Text -> M.Map [T.Text] Integer -> State [T.Text] (M.Map [T.Text] Integer)
parseLine cmd fileSizes
  | cdHome == cmd = do
    put []
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
    let (sz, name) = T.break (/= ' ') cmd
    curr <- get
    let fullPath = name : curr
    return $ M.insert fullPath (read (T.unpack sz) :: Integer) fileSizes

main :: IO ()
main = do
  contents <- T.lines <$> TIO.readFile "sample_input.txt"
  let f = flip parseLine
  let (fileSizes, _) = runState (foldM f (M.empty :: M.Map [T.Text] Integer) contents) []
  print fileSizes
