import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.State

--TODO: Update this to use Data.Text

parseLine2 :: [String] -> M.Map [String] Integer -> State [String] (M.Map [String] Integer)
parseLine2 ("$ cd ":dir) fileSizes = do
                                curr <- get
                                put (head dir : curr)
                                return fileSizes


{-
parseLine3 :: [String] -> M.Map [String] Integer -> State [String] (M.Map [String] Integer)
parseLine3 cmd fileSizes = do
                                curr <- get
                                put cmd : curr
                                return fileSizes
-}
{-
-- I need several functions that return a State. No functions will accept a state as a parameter
parseLine :: [String] -> State (String, M.Map [String] Integer) ()
parseLine ("$":_) = do 
    let x = 5
    let y = M.fromList [([""], 3)]
    put ("test", y)
    return ()
    -}

main :: IO ()
main = do
    print "test"
