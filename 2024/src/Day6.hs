{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Grid

import Parsing
import Parsing.Grid
import Data.Biapplicative
import Data.List (unfoldr)
import Control.Comonad
import Control.Monad
    ( (<=<)
    , join
    -- , when
    )
import Data.Array.IArray (assocs)
import qualified Data.Set as Set

main :: IO ()
main = do
    roomMapUnfocused <- parseOrFail parseCharGrid "inputs/day6.txt"
    roomMap <- maybe (fail "cannot find guard position") pure $ findGuardPos roomMapUnfocused
    let allSteps = unfoldr (fmap (join bipure) . doGuardStep) roomMap
        stepsSet = Set.fromList $ fmap pos allSteps
    print $ Set.size stepsSet

prettyGrid :: Grid Char -> String
prettyGrid = foldr (\ ((_,col),a) s -> if col == 0 then '\n':a:s else a:s) "" . assocs . cells

findGuardPos :: Grid Char -> Maybe (Grid Char)
findGuardPos = trySeekBy (== '^') 

doGuardStep :: Grid Char -> Maybe (Grid Char)
doGuardStep g = do
    dir <- currentDir g
    let nextPos = move dir (pos g)
    nextCell <- tryPeek nextPos g
    case nextCell of
      '#' -> editM (dir2Chr . rotCW <=< chr2Dir) g
      '.' -> (fmap (set (extract g)) . trySeek nextPos . set '.') g
      _   -> Nothing

newtype Direction = Dir (Int, Int)
    deriving (Eq, Show)

currentDir :: Grid Char -> Maybe Direction
currentDir = chr2Dir . extract

move :: Direction -> (Int, Int) -> (Int, Int)
move (Dir x) = biLiftA2 (+) (+) x

chr2Dir :: Char -> Maybe Direction
chr2Dir '^' = Just up
chr2Dir '>' = Just right
chr2Dir 'v' = Just down
chr2Dir '<' = Just left
chr2Dir _ = Nothing

dir2Chr :: Direction -> Maybe Char
dir2Chr c
  | c == up = Just '^'
  | c == right = Just '>'
  | c == down = Just 'v'
  | c == left = Just '<'
  | otherwise = Nothing

up, down, left, right :: Direction
up = Dir (-1, 0)
down = Dir (1, 0)
left = Dir (0, -1)
right = Dir (0, 1)

allDirs :: [Direction]
allDirs = [up, left, down, right]

rotCW :: Direction -> Direction
rotCW (Dir (v, h)) = Dir (h, (-v))

rotCCW :: Direction -> Direction
rotCCW (Dir (v, h)) = Dir ((-h), v)