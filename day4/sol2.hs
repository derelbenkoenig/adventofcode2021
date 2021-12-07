import Data.List
import Data.Monoid
import Data.Maybe

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

-- if the board already scored, just keep that score instead of replacing it with a new, worse one
applyNumToBoard n board = (newBoard, newScore) where
        newBoard = (map . map) (\(x, b) -> (x, b || (x == n))) $ board
        newScore = tryScore n newBoard

applyNumToAllBoards n (scoredBoards, unscoredBoards) = let
    (newlyScoredBoards, stillUnscoredBoards) = partition (isJust . snd) $ map (applyNumToBoard n) unscoredBoards
    newlyScored' = map (\(x, Just n) -> (x, n)) newlyScoredBoards
    stillUnscored' = map (\ (x, Nothing) -> x) stillUnscoredBoards
    in (scoredBoards ++ newlyScored', stillUnscored')

hasScored board = hasScoredRow board || hasScoredCol board where
    hasScoredRow = any $ all snd
    hasScoredCol = hasScoredRow . transpose

-- sum up the NON marked spaces and multiply that sum by N.
tryScore :: Int -> Board -> Maybe Int
tryScore n board = if hasScored board then Just $ (* n) $ sum $ map (\(x,b) -> if b then 0 else x) $ concat board else Nothing

playGame :: [Int] -> [Board] -> Int
playGame nums boards = let
    initialState = ([], boards)
    (boardScores, _) = foldl (flip applyNumToAllBoards) initialState nums
    (_,score) =  last boardScores
    in score


process = uncurry playGame . parseInput

main = getContents >>= (return . process) >>= print
