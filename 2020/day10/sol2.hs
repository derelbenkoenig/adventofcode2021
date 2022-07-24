import Data.List
import Data.Array.IArray
import Data.Maybe

when p f x = if p x then Just (f x) else Nothing

tryGet :: (Ix i) => Array i e -> i -> Maybe e
tryGet as = when (inRange (bounds as)) (as !)

takeWhileJust xs = case xs of
    [] -> []
    Nothing:_ -> []
    (Just x):xs' -> x:(takeWhileJust xs')

filterMaybe p mx = case mx of
    Nothing -> Nothing
    Just x -> if p x then Just x else Nothing

solve :: [Integer] -> Integer
solve xs = let
    inputList :: [Integer]
    inputList = sort $ (3 + maximum xs):0:xs
    rg :: (Int, Int)
    rg = (1, length inputList)
    inputArr :: Array Int Integer
    inputArr = listArray rg inputList
    ancestors :: Int -> Array Int Integer -> Integer
    ancestors n arr = let
        atN = inputArr ! n
        indices' :: [Int]
        indices' = map (n -) [1..]
        indices :: [Int]
        indices = takeWhile (maybe False (>= atN - 3) . tryGet inputArr) indices'
        vals = map (arr !) indices
        in sum vals
    addIth :: Array Int Integer -> Int -> Array Int Integer
    addIth arr i = arr // [(i, ancestors i arr)]
    table :: Array Int Integer
    table = foldl' addIth (array rg [(1,1)]) (drop 1 $ range rg)
    in table ! (snd (rg))

main :: IO ()
main = interact ((++ "\n") . show . solve . map read . lines)
