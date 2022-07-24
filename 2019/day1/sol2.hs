import Data.Array.IArray
import Data.List

fuelForMass m = max 0 $ div m 3 - 2

massSequence = takeWhile (> 0) . drop 1 . iterate fuelForMass

solve :: [Int] -> Int
solve masses = sum $ map (sum . massSequence) masses

readInput :: String -> [Int]
readInput = map read . lines

main :: IO ()
main = interact ((++ "\n") . show . solve . readInput)
