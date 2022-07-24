import Data.Foldable
import Data.List

fromBinary s = let
    bitval b = case b of
        '0' -> 0
        '1' -> 1
        otherwise -> undefined
    in foldl' (\acc bit -> acc * 2 + bitval bit) 0 s

mostCommonBit s = let
    (zerosCount, onesCount) = go1 (0,0) s where
        go1 (x,y) ('0':s') = go1 (x + 1, y) s'
        go1 (x,y) ('1':s') = go1 (x, y + 1) s'
        go1 (x,y) [] = (x,y)
    in if zerosCount > onesCount then '0' else '1'

process s = let
    gammaBits = map mostCommonBit . transpose . lines $ s
    epsilonBits = map bitComplement gammaBits where
        bitComplement '0' = '1'
        bitComplement '1' = '0'
    gammaValue = fromBinary gammaBits
    epsilonValue = fromBinary epsilonBits
    in gammaValue * epsilonValue

main = getContents >>= (return . process) >>= print
