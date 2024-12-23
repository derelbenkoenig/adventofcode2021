module Parsing.Grid
    ( parseCharGrid
    )
    where

import Parsing
import Control.Applicative hiding (many)
import Data.Grid

parseCharGrid :: StatefulParser ((Int, Int), (Int, Int)) (Grid Char)
parseCharGrid = do
    put ((0,0), (0,0))
    indexedList <- many $ do
        ((row, col), (maxRow, maxCol)) <- get
        charOrEol <- eitherP eol anySingle
        case charOrEol of
          Left _ -> do
              let newRow = row + 1
              put ((newRow, 0), (max row maxRow, maxCol)) 
              empty
          Right c -> do
              let newCol = col + 1
              put ((row, newCol), (maxRow, max col maxCol))
              pure ((row, col), c)
    (_, maxIndex) <- get
    pure $ grid ((0,0), maxIndex) indexedList

-- parseWordSearch :: StatefulParser (Int, Int) (Grid Char)
-- parseWordSearch = do
--     let parseLine = T.unpack <$> (takeWhileP (Just "uppercase") isUpper <* eol)
--     line <- parseLine
--     put (1, length line)
--     restLines <- many $ do
--         l <- parseLine
--         modify $ \ (rows, cols) -> (rows + 1, max cols (length l))
--         pure l
--     let indexedLines =
--             zipWith (\ i ln -> zipWith (\ j c -> ((i,j), c)) [0..] ln) [0..] (line:restLines)
--     (rows, cols) <- get
--     pure $ grid ((0,0), (rows - 1, cols - 1)) $ concat indexedLines
