import Data.Char
import Data.Foldable

data MvCmd = Forward Int | Down Int | Up Int
    deriving (Read, Show)

casify s = case s of
    [] -> []
    c:cs -> (toUpper c):cs

doMove (row,col) cmd = case cmd of
    Forward x -> (row, col + x)
    Down x -> (row + x, col)
    Up x -> (row - x, col)

finalPos = foldl' doMove (0,0)

process = (uncurry (*)) . finalPos . map (read . casify) . lines

main = getContents >>= (return . process) >>= print
