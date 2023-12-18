{-# LANGUAGE OverloadedLists #-}

module Problems.AllDays where

import Data.Vector (Vector)
import Problems
import Problems.Day1 (day1)

allDays :: Vector PrintableDay
allDays =
    [
        PrintableDay day1
    ]
