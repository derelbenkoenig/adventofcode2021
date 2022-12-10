module Aoc
    ( module Solutions,
      module Day1,
      runSolution,
    ) where

import Solutions

import qualified Day1

runSolution :: ProblemNumber -> FilePath -> IO ()
runSolution (ProblemNumber d p) = case d of
    1 -> Day1.runSolution p
    _ -> const $ fail $ "no solution for day " <> show d <> " problem " <> show p
