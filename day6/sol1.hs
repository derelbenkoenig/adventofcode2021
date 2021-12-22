import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

nextDay :: [Int] -> [Int]
nextDay fishes = let
    go olds news todays = case todays of
        [] -> olds ++ news
        (x:xs) -> let (nextOlds,nextNews) = if x == 0 then (6:olds, 8:news) else ((x-1):olds, news)
            in go nextOlds nextNews xs
    in go [] [] fishes

intListParser :: ReadP [Int]
intListParser = do
    ints <- sepBy getInt (skipSpaces >> char ',' >> skipSpaces) 
    skipSpaces
    eof
    return ints
    where getInt = fmap read $ munch1 isDigit

nTimes :: Integral i => i -> (a -> a) -> a -> a
nTimes 0 _ x             = x
nTimes n f x | n > 0     = nTimes (n - 1) f $! f x
               | otherwise = error "nTimes: Negative input."

processInput input = let 
    [(fishes,"")] = readP_to_S intListParser input
    in length $ nTimes 80 nextDay fishes

main = getContents >>= (return . processInput) >>= print
