import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Array.IO
import Data.Array
import Data.Foldable

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

sortPair (a,b) = if a <= b then (a,b) else (b,a)

rangeFromVentLine (VentLine x1 y1 x2 y2) = zip (fromto x1 x2) (fromto y1 y2) where
    fromto a b = case compare a b of
      LT -> [a..b] 
      GT -> reverse [b..a]
      EQ -> repeat a

-- index is (Int, Int), element is Int
type Board = IOArray (Int, Int) Int

type FrozenBoard = Array (Int, Int) Int

blankBoard :: Int -> Int -> IO Board
blankBoard numRows numCols = newArray ((0,0),(numRows,numCols)) 0

markBoard :: [VentLine] -> Board -> IO Board
markBoard ventLines startingBoard = foldlM markLine startingBoard ventLines

markLine :: Board -> VentLine -> IO Board
markLine board vl = foldlM markSquare board (rangeFromVentLine vl)

markSquare :: Board -> (Int,Int) -> IO Board
markSquare b (x,y) = do
    oldVal <- readArray b (x,y)
    let newVal = oldVal + 1
    writeArray b (x,y) newVal
    return b

countBadSquares :: Board -> IO Int
countBadSquares board = frozen >>= (return . foldl (\acc sq -> acc + fromEnum (sq >= 2)) 0) where
    frozen = (freeze board :: IO FrozenBoard)

processInput :: String -> IO Int
processInput input = let
    [(ventLines,"")] = readP_to_S inputParser input
    (numCols, numRows) = foldl' (\ (x, y) (VentLine x1 y1 x2 y2) -> ((max x (max x1 x2)) + 1, (max y (max y1 y2)) + 1)) (0,0) ventLines
    markedBoard = markBoard ventLines =<< (blankBoard numRows numCols)
    in markedBoard >>= countBadSquares

main = getContents >>= processInput >>= print
