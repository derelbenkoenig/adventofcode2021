module Problems.Day1 where

import Data.Char (isDigit, digitToInt)
import qualified Data.Text as T
import qualified Data.Vector as V
import Problems

day1 :: Day Int Int
day1 = Day p1 p2

p1 :: Solution Int
p1 = textSolution $
    pure . sum . map (uncurry twoDigitsToNum . firstAndLastDigit) . T.lines

firstAndLastDigit :: T.Text -> (Char, Char)
firstAndLastDigit t = let digits = T.filter isDigit t
    in (T.head digits, T.last digits)

twoDigitsToNum :: Char -> Char -> Int
twoDigitsToNum x y = 10 * digitToInt x + digitToInt y

p2 :: Solution Int
p2 = undefined

textToVector :: T.Text -> V.Vector Char
textToVector t = V.generate (T.length t) (T.index t)

spelledDigits :: [(String, Int)]
spelledDigits =
    [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
    ]

