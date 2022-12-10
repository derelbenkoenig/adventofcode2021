module Aoc
    ( module Solutions,
      runSolution,
    ) where

import Solutions

import qualified Day1
import qualified Day2
import qualified Day3

runSolution :: ProblemNumber -> FilePath -> IO ()
runSolution (ProblemNumber d p) = case d of
    1 -> Day1.runSolution p
    2 -> Day2.runSolution p
    3 -> Day3.runSolution p
    _ -> const $ fail $ "no solution for day " <> show d <> " problem " <> show p