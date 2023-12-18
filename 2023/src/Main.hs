module Main (main) where

import Options.Applicative
import Problems
import Problems.AllDays
import Data.Vector ((!))

main :: IO ()
main = do
    (dayNum, partNum) <- execParser
        (flip info
            (header "run advent of code solution")
            $ helper <*>
                ((,)
                    <$> argument auto (metavar "DAY")
                    <*> argument auto (metavar "PART")
                )
        )
    let day = allDays ! (dayNum - 1)
    let solution = getPartNum partNum day

    computeAndPrintAnswer solution ("inputs/day" ++ show dayNum ++ "/input")
