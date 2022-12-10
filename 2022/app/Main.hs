{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aoc
import Numeric.Natural
import Options.Applicative

main :: IO ()
main = do
    problemNumber@(ProblemNumber d p) <- execParser (info (helper <*> liftA2 ProblemNumber day problem) myInfoMod)
    let inFile = "inputs/day" <> show d <> "/input.txt"
    putStrLn $ "doing problem " <> show (d,p) <> " with inFile " <> inFile
    runSolution problemNumber inFile

myInfoMod :: InfoMod a
myInfoMod = fullDesc <> header "execute advent of code problem"

day :: Parser Natural
day = option auto (
    long "day"
    <> short 'd'
    <> metavar "DAY"
    <> help "which day's problem to run")

problem :: Parser Natural
problem = option (auto >>= validateProblem) (
    long "problem"
    <> short 'p'
    <> metavar "PROBLEM"
    <> help "which problem of the day to run (1 or 2)")

validateProblem :: MonadFail m => Natural -> m Natural
validateProblem p = if p `elem` [1,2]
                        then return p
                        else fail $ "problem must be 1 or 2, not " ++ show p
