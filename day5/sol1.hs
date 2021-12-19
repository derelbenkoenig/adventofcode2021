import Text.ParserCombinators.ReadP
import Data.Char
import Data.List

data VentLine = VentLine Int Int Int Int

instance Show VentLine where
    show (VentLine x1 y1 x2 y2) = "VentLine ("++(show x1)++","++(show y1)++") -> ("++(show x2)++","++(show y2)++")"

-- the input is getting complex enough to be worth using parser combinators, I think
ventLineParser :: ReadP VentLine
ventLineParser = do
    x1 <- getInt
    char ','
    y1 <- getInt
    skipSpaces
    string "->"
    skipSpaces
    x2 <- getInt
    char ','
    y2 <- getInt
    return $ VentLine x1 y1 x2 y2
    where getInt = fmap read $ munch1 isDigit

inputParser :: ReadP [VentLine]
inputParser = manyTill (ventLineParser >>= \x -> (char '\n') >> return x) eof

isHorizontal (VentLine x1 y1 x2 y2) = y1 == y2
isVertical   (VentLine x1 y1 x2 y2) = x1 == x2

isHoriOrVert vl = isHorizontal vl || isVertical vl

blankBoard numRows numCols= replicate numRows $ replicate numCols 0

markBoard ventLines startingBoard = foldl' markLine startingBoard (filter isHoriOrVert ventLines) where

sortPair (x,y) = if x < y then (x,y) else (y,x)
boundedBy bound1 bound2 x = x >= lowerBound && x <= upperBound where (lowerBound,upperBound) = sortPair (bound1,bound2)

markLine board vl = map (markByCoords vl) (addCoords board) where
    addCoords rows = zipWith (\ y row -> zipWith (\ x square -> ((x, y), square)) [0..] row) [0..] rows
    markByCoords (VentLine x1 y1 x2 y2) = map (\ ((x,y),sq) -> sq + (fromEnum (boundedBy x1 x2 x && boundedBy y1 y2 y)))

countBadSquares board = sum $ map (fromEnum . (>= 2)) $ concat board

processInput input = let
    [(ventLines,"")] = readP_to_S inputParser input
    (numCols, numRows) = foldl' (\ (x, y) (VentLine x1 y1 x2 y2) -> ((max x (max x1 x2)) , (max y (max y1 y2)))) (0,0) ventLines
    markedBoard =  markBoard ventLines (blankBoard numRows numCols)
    in countBadSquares markedBoard

main = getContents >>= (return . processInput) >>= print
