import Control.Monad.State
import qualified Data.Maybe as MB
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T

cdPrefix = T.pack "$ cd" :: T.Text
lsPrefix = T.pack "$ ls" :: T.Text

parseLine ::
     T.Text -> M.Map [T.Text] Integer -> State [T.Text] (M.Map [T.Text] Integer)
parseLine cmd fileSizes
  | T.isPrefixOf cdPrefix cmd = do
    let dir = T.splitOn (T.pack ",") . MB.fromJust $ T.stripPrefix cdPrefix cmd
    curr <- get
    put (dir : curr)
    return fileSizes
  | T.isPrefixOf lsPrefix cmd = do
    let dir = cmd
    curr <- get
    put (dir : curr)
    return fileSizes

{-
parseLine3 :: [T.Text] -> M.Map [T.Text] Integer -> State [T.Text] (M.Map [T.Text] Integer)
parseLine3 cmd fileSizes = do
                                curr <- get
                                put cmd : curr
                                return fileSizes
-}
{-
-- I need several functions that return a State. No functions will accept a state as a parameter
parseLine :: [T.Text] -> State (T.Text, M.Map [T.Text] Integer) ()
parseLine ("$":_) = do 
    let x = 5
    let y = M.fromList [([""], 3)]
    put ("test", y)
    return ()
    -}
main :: IO ()
main = do
  print "test"
