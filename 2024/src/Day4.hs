{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parsing
import Data.Char (isUpper)
import Data.Grid
import qualified Data.Text as T
import Data.Bifunctor
import Control.Monad (guard)
import Control.Comonad

main :: IO ()
main = do
    (wordSearch, _) <- parseOrFailStateful parseWordSearch "inputs/day4.txt" (0,0)
    print $ sum $ extend (findWordAnyDirection "XMAS") wordSearch

type NeighborFinder = Grid Char -> (Int, Int) -> [(Int, Int)]

mkNeighborFinder :: ((Int, Int) -> (Int, Int)) -> NeighborFinder
mkNeighborFinder f g ix = filter (inBounds g) $ take 4 $ iterate f ix

neighborFinders :: [NeighborFinder]
neighborFinders = do
    rowMv <- [-1, 0, 1]
    colMv <- [-1, 0, 1]
    guard $ rowMv /= 0 || colMv /= 0
    pure $ mkNeighborFinder $ bimap (+ rowMv) (+ colMv)

findWord :: String -> Grid Char -> NeighborFinder -> Bool
findWord w g nf = experiment (nf g) g == w

findWordAnyDirection :: String -> Grid Char -> Int
findWordAnyDirection w g = sum $ fmap (fromEnum . (findWord w g)) neighborFinders

parseWordSearch :: StatefulParser (Int, Int) (Grid Char)
parseWordSearch = do
    let parseLine = T.unpack <$> (takeWhileP (Just "uppercase") isUpper <* eol)
    line <- parseLine
    put (1, length line)
    restLines <- many $ do
        l <- parseLine
        modify $ \ (rows, cols) -> (rows + 1, max cols (length l))
        pure l
    let indexedLines =
            zipWith (\ i ln -> zipWith (\ j c -> ((i,j), c)) [0..] ln) [0..] (line:restLines)
    (rows, cols) <- get
    pure $ grid ((0,0), (rows - 1, cols - 1)) $ concat indexedLines
