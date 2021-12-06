{-# LANGUAGE TupleSections #-}

import Data.List
import Data.Monoid

batch n xs = let (ys, zs) = splitAt n xs in ys:(if null zs then [] else batch n zs)

type Board = [[(Int, Bool)]]

splitBy c s = case span (/= c) s of
    (firstWord,[]) -> [firstWord]
    (firstWord,xs) -> firstWord:(splitBy c $ drop 1 xs)

parseInput :: String -> ([Int], [Board])
parseInput input = (nums, boards) where
    inputLines = lines input
    (numsLine, rest) = splitAt 2 inputLines
    nums = map read $ splitBy ',' $ head numsLine
    boards = (map (map rowFromLine) . map (take 5) . batch 6) rest where
        rowFromLine =  map ((\x -> (x, False)) . read) . words

applyNumToBoard n = (map . map) (\(x, b) -> (x, b || (x == n)))

hasScored board = hasScoredRow board || hasScoredCol board where
    hasScoredRow = any $ all snd
    hasScoredCol = hasScoredRow . transpose

-- sum up the NON marked spaces
tryScore :: Int -> Board -> Maybe Int
tryScore n board = if hasScored board then Just $ (* n) $ sum $ map (\(x,b) -> if b then 0 else x) $ concat board else Nothing

tryFindFirstScored :: Int -> [Board] -> Maybe Int
tryFindFirstScored n = getFirst . mconcat . map (First . tryScore n)

-- for each num, apply it to all boards, see if one scored, if so, short circuit and return the score
playGame :: [Int] -> [Board] -> Maybe Int
playGame nums boards = let
    states :: [(Int, [Board])]
    states = zip (0:nums) $ scanl (\bs n -> map (applyNumToBoard n) bs) boards nums
    in getFirst . mconcat . map (First . uncurry tryFindFirstScored) $ states

process = uncurry playGame . parseInput

main = getContents >>= (return . process) >>= print
