module Parsing.Grid
    ( parseCharGrid
    )
    where

import Parsing
import Control.Applicative hiding (many)
import Data.Grid
import Data.Bifunctor
import Data.Monoid (Endo(..))
import qualified Data.Text as T

parseCharGrid :: Parser (Grid Char)
parseCharGrid = do
    firstLine <- parseLine
    let lineLength = T.length firstLine
    restLines <- many parseLine
    let lines = firstLine : restLines
        numLines = length lines
        allChars = T.unpack $ T.concat lines
        rowIndices = replicate lineLength 0 ++ fmap (+ 1) rowIndices
        colIndices = cycle [0..lineLength - 1]
        indices = zip rowIndices colIndices
        indexedChars = zip indices allChars
    pure $ grid ((0, 0), (lineLength - 1, numLines - 1)) indexedChars

parseLine :: Parser Text
parseLine = takeWhile1P Nothing (not . isEol) <* eol

isEol :: Char -> Bool
isEol = liftA2 (||) (== '\n') (== '\r')
