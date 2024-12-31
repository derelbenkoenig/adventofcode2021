module Parsing.Grid
    ( parseCharGrid
    )
    where

import Parsing
import Control.Applicative hiding (many)
import Data.Grid
import Control.Monad (when)
import qualified Data.Text as T

parseCharGrid :: Parser (Grid Char)
parseCharGrid = do
    firstLine <- parseLine
    let lineLength = T.length firstLine
    restLines <- many $ do
        nextLine <- parseLine
        when (T.length nextLine /= lineLength) $ fail "mismatched line length"
        pure nextLine
    let allLines = firstLine : restLines
        numLines = length allLines
        allChars = T.unpack $ T.concat allLines
        indices = liftA2 (,) [0..numLines - 1] [0..lineLength - 1]
        indexedChars = zip indices allChars
    pure $ grid ((0, 0), (numLines - 1, lineLength - 1)) indexedChars

parseLine :: Parser Text
parseLine = takeWhile1P Nothing (not . isEol) <* eol

isEol :: Char -> Bool
isEol = liftA2 (||) (== '\n') (== '\r')
