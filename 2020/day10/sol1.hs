import Data.List

mapAdj :: (a -> a -> b) -> [a] -> [b]
mapAdj f xs = zipWith f xs (drop 1 xs)

diffs = mapAdj (flip (-))

counts :: (Foldable t, Integral n) => t n -> (n, n)
counts = foldl' f (0, 1) where
    f (ones, threes) x = case x of
        1 -> (ones + 1, threes)
        3 -> (ones, threes + 1)
        otherwise -> (ones, threes)

solve = uncurry (*) . counts . diffs . (0 :) . sort

main :: IO ()
main = interact ((++ "\n") . show . solve . map (read :: String -> Int)  . lines)
