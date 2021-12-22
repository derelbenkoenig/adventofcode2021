import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

example = [16,1,2,0,4,2,7,1,2,14]

intListParser :: ReadP [Int]
intListParser = do
    ints <- sepBy getInt (skipSpaces >> char ',' >> skipSpaces) 
    skipSpaces
    eof
    return ints
    where getInt = fmap read $ munch1 isDigit

-- mean l = (fromIntegral $ sum l) / (fromIntegral $ length l)

-- costs l = let
--     meanPos = round (mean l :: Double)
--     in map (\x -> abs (x - meanPos)) l

costsFrom pos l = map (costToMoveTo pos) l where
    costToMoveTo a b = triangle (abs (a - b))

triangle n = (n + 1) * n `div` 2

bestCost l = minimum $ map (\pos -> sum $ costsFrom pos l) [(minimum l)..(maximum l)]

processInput input = let
    [(positions,"")] = readP_to_S intListParser input
    in bestCost positions

main = getContents >>= (return . processInput) >>= print
