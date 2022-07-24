import Data.Char
import Data.Foldable

data MvCmd = Forward Int | Down Int | Up Int
    deriving (Read, Show)

casify s = case s of
    [] -> []
    c:cs -> (toUpper c):cs

doMove (row,col,aim) cmd = case cmd of
    Forward x -> (row + x * aim, col + x, aim)
    Down x -> (row, col, aim + x)
    Up x -> (row, col, aim - x)

finalPos = foldl' doMove (0,0,0)

process = (\(r,c,a) -> r * c) . finalPos . map (read . casify) . lines

main = getContents >>= (return . process) >>= print
