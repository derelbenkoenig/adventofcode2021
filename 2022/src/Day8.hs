{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day8 where

import Control.Applicative hiding (many, some)
import Control.Monad (MonadPlus(..))
import Data.Array.IArray
import Data.Char (digitToInt)
import Data.Foldable (foldl', foldMap', maximumBy)
import Data.Function (on)
import Data.List.Church.Left.Strict
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution n fp = do
    input <- T.readFile fp
    forest <- parseOrFail parseTrees fp input
    case n of
        1 -> do 
            let visibleSet = visibleTreeCoords forest
            print $ S.size visibleSet
        2 -> do
            let bestIx = maximumBy (compare `on` scenicScore forest) (indices forest)
            print $ scenicScore forest bestIx
        _ -> fail "problem must be 1 or 2"

type TreeIx = (Int, Int)
type Forest = Array TreeIx Int

data ForestBounds = ForestBounds
    { rmin :: Int,
      cmin :: Int,
      rmax :: Int,
      cmax :: Int }

forestBounds :: Forest -> ForestBounds
forestBounds forest =
    let ((rmin, cmin), (rmax, cmax)) = bounds forest
        in ForestBounds{..}

visibleTreeCoords :: Forest -> S.Set TreeIx
visibleTreeCoords forest =
    let ForestBounds{..} = forestBounds forest
        row, revRow, col, revCol :: Int -> [TreeIx]
        row r = map (r, ) [cmin,cmin+1..cmax]
        revRow r = map (r, ) [cmax,cmax-1..cmin]
        col c = map (, c) [rmin,rmin+1..rmax]
        revCol c = map (, c) [rmax,rmax-1..rmin]

        rows, revRows, cols, revCols :: [[TreeIx]]
        rows    = map row    [rmin..rmax]
        revRows = map revRow [rmin..rmax]
        cols    = map col    [cmin..cmax]
        revCols = map revCol [cmin..cmax]

        allLines :: FoldLList [TreeIx]
        allLines = foldMap' foldLList [rows, revRows, cols, revCols]

        foldLine :: S.Set TreeIx -> [TreeIx] -> S.Set TreeIx
        foldLine s = fst . foldl' (accumVisible forest) (s, -1)

        in foldLFoldl' foldLine S.empty allLines

-- takeWhile, but it includes the first element that does not satisfy the predicate,
-- but none after that
takeWhileThru :: (a -> Bool) -> [a] -> [a]
takeWhileThru _ [] = []
takeWhileThru p (x:xs) =
    if p x
        then x:(takeWhileThru p xs)
        else [x]

scenicScore :: Forest -> TreeIx -> Int
scenicScore forest (r, c) =
    let ForestBounds{..} = forestBounds forest
        viewLeft  = map (r,) [c-1,c-2..cmin]
        viewRight = map (r,) [c+1,c+2..cmax]
        viewUp    = map (,c) [r-1,r-2..rmin]
        viewDown  = map (,c) [r+1,r+2..rmax]
        viewScore = length . takeWhileThru (\ix -> forest ! ix < forest ! (r, c))
        in product $ map viewScore [viewLeft, viewRight, viewUp, viewDown]

accumVisible :: Forest -> (S.Set TreeIx, Int) -> TreeIx -> (S.Set TreeIx, Int)
accumVisible forest (oldVis, tallest) new =
    let newTree = forest ! new 
        in if newTree > tallest
              then (S.insert new oldVis, newTree)
              else (oldVis, tallest)

parseTrees :: Parser Forest
parseTrees = do
    (treesAsLists, height) <- someCounting (someCounting (digitToInt <$> digitChar) <* eol)
    let width = snd $ head treesAsLists
    return $ listArray ((1, 1), (height, width)) (concatMap fst treesAsLists)

manyCounting :: MonadPlus m => m a -> m ([a], Int)
manyCounting p = go id
  where
    go f = do
      r <- optional p
      case r of
        Nothing -> return (f ([], 0))
        Just x -> go (f . \(xs, ct) -> (x:xs, ct+1))

someCounting :: MonadPlus m => m a -> m ([a], Int)
someCounting p = liftA2 (\x (xs, ct) -> (x:xs, ct+1)) p (manyCounting p)
