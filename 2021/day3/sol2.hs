import Data.Foldable
import Data.List

fromBinary s = let
    bitval b = case b of
        '0' -> 0
        '1' -> 1
        otherwise -> undefined
    in foldl' (\acc bit -> acc * 2 + bitval bit) 0 s

bitComplement '0' = '1'
bitComplement '1' = '0'

-- from problem description, ones should win in a tie. So only return '0' if zeros strictly greater.
mostCommonBit s = let
    (zerosCount, onesCount) = go1 (0,0) s where
        go1 (x,y) ('0':s') = go1 (x + 1, y) s'
        go1 (x,y) ('1':s') = go1 (x, y + 1) s'
        go1 (x,y) [] = (x,y)
    in if zerosCount > onesCount then '0' else '1'

leastCommonBit = bitComplement . mostCommonBit

findByBitCriterion :: (String -> Char) -> [String] -> String
findByBitCriterion criterion ss = let
    go :: [(String, String)] -> String
    go pairs = case pairs of
        (s,_):[] -> s
        pairs' -> let
            whichBit = criterion $ map (head . snd) pairs'
            candidates = filter ((== whichBit) . head . snd) pairs'
            in go $ map (\(x,y) -> (x, tail y)) candidates
    in go $ map (\x -> (x,x)) ss

oxygenVal = findByBitCriterion mostCommonBit
co2Val = findByBitCriterion leastCommonBit

lifeSupport inputs = (fromBinary $ oxygenVal inputs) * (fromBinary $ co2Val inputs)

process = lifeSupport . words

main = getContents >>= (return . process) >>= print
