module Day1 where
    
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Numeric.Natural
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
 
type MyParser a = Parsec Void T.Text a

parser :: MyParser [[Int]]
parser = (decimal `sepEndBy` eol) `sepEndBy` eol

runSolution :: Natural -> FilePath -> IO ()
runSolution 1 = processSnackBundles biggestSnackBundle
runSolution 2 = processSnackBundles (sum . topNSnackBundles 3)

processSnackBundles :: Show a => ([[Int]] -> a) -> FilePath -> IO ()
processSnackBundles f fp = do
    input <- T.readFile fp
    snackBundles <- parseOrFail parser fp input
    print $ f snackBundles


biggestSnackBundle :: [[Int]] -> Int
biggestSnackBundle = maximum . map sum

topNSnackBundles n = foldl' (accumMaxN n) [] . map sum

accumMaxN :: Ord a => Int -> [a] -> a -> [a]
accumMaxN 0 _ _ = []
accumMaxN n [] a = [a]
accumMaxN n (m:ms) a = bigger:(accumMaxN (n - 1) ms smaller) where
    (bigger, smaller) = if a > m then (a, m) else (m, a)
