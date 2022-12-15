module Aoc
    ( module Solutions,
      runSolution,
    ) where

import Solutions hiding (Parser)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

runSolution :: ProblemNumber -> FilePath -> IO ()
runSolution (ProblemNumber d p) = case d of
    1 -> Day1.runSolution p
    2 -> Day2.runSolution p
    3 -> Day3.runSolution p
    4 -> Day4.runSolution p
    5 -> Day5.runSolution p
    6 -> Day6.runSolution p
    7 -> Day7.runSolution p
    8 -> Day8.runSolution p
    _ -> const $ fail $ "no solution for day " <> show d <> " problem " <> show p
