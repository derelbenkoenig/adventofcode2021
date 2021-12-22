import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Array.IArray

intListParser :: ReadP [Int]
intListParser = do
    ints <- sepBy getInt (skipSpaces >> char ',' >> skipSpaces)
    skipSpaces
    eof
    return ints
    where getInt = fmap read $ munch1 isDigit

-- represent all the fishes as an array with indices 0..8 (inclusive) of how many fish are at that level
fish2Arr :: [Int] -> Array Int Int
fish2Arr fishes = let
    startingArr = listArray (0,8) (repeat 0) :: Array Int Int
    in foldl' (\a i -> let x = a ! i in a // [(i,x+1)]) startingArr fishes

nextDay fishArr = fishArr // updateList where
    updateList = newborns:newSixes:living
    -- new 8 and new 6 are calculated specially *from 0 and 7*
    living = [(nextNum i, fishArr ! i) | i <- [1,2,3,4,5,6,8]]
    nextNum i = if i == 0 then 6 else i-1
    newborns = (8, fishArr ! 0)
    newSixes = (6, fishArr ! 0 + fishArr ! 7)

-- I stole this from somewhere but it works. Unfortunately still way slower than like, a for loop
nTimes :: Integral i => i -> (a -> a) -> a -> a
nTimes 0 _ x             = x
nTimes n f x | n > 0     = nTimes (n - 1) f $! f x
               | otherwise = error "nTimes: Negative input."

processInput input = let
    [(fishes,"")] = readP_to_S intListParser input
    fishArr = fish2Arr fishes
    endResult = nTimes 256 nextDay fishArr
    in (endResult, sum endResult)

main = getContents >>= (return . processInput) >>= print
