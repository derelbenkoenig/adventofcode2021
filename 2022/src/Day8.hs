{-# LANGUAGE TupleSections #-}

module Day8 where

import Control.Applicative hiding (many, some)
import Control.Monad (MonadPlus(..))
import Data.Array.IArray
import Data.Char (digitToInt)
import Data.Foldable (foldl', foldMap')
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
    let visibleSet = visibleTreeCoords forest
    print $ S.size visibleSet

type TreeIx = (Int, Int)
type Forest = Array TreeIx Int

-- I think they call this one a Church-encoded list
newtype FoldLList a = FoldLList (forall b. (b -> a -> b) -> b -> b)

instance Show a => Show (FoldLList a) where
    showsPrec d as = showParen (d > 10) $
        showString "foldLList " . showsPrec 11 (foldLToList as)

foldLCons :: a -> FoldLList a -> FoldLList a
foldLCons a (FoldLList f) = FoldLList $ \g z -> f g (z `g` a)

foldLSnoc :: FoldLList a -> a -> FoldLList a
foldLSnoc (FoldLList f) a = FoldLList $ \g z -> f g z `g` a

foldLList :: [a] -> FoldLList a
foldLList as = FoldLList $ \f z -> foldl' f z as

snoc :: [a] -> a -> [a]
snoc [] a = [a]
snoc (x:xs) a = x:(snoc xs a)

foldLToList :: FoldLList a -> [a]
foldLToList (FoldLList f) = f snoc []

foldLEmpty :: FoldLList a
foldLEmpty = FoldLList $ \f z -> z

foldLAppend :: FoldLList a -> FoldLList a -> FoldLList a
foldLAppend (FoldLList f) (FoldLList g) = FoldLList $ \h z -> g h (f h z)

foldLFoldl' :: (b -> a -> b) -> b -> FoldLList a -> b
foldLFoldl' g z (FoldLList f) = f g z

instance Semigroup (FoldLList a) where
    (<>) = foldLAppend

instance Monoid (FoldLList a) where
    mempty = foldLEmpty

{-
-- some point-free fun:
-- h = foldl function, f = fmap function, a = accumulator, x = element
c2 f h a x = h a (f x)
c2 f h a   = h a . f
c2 f h a   = (.) (h a) f
c2 f h a   = flip (.) f (h a)
c2 f h     = (flip (.) f) . h
c2 f h     = (. f) . h -- I actually stop here since use site has f and h, and this looks nice
c2 f       = ((. f) .)
c2 f       = (.) (. f)
c2 f       = (.) (flip (.) f)
c2         = (.) . (flip (.))

foldl h z . fmap f = foldl (c2 f h) z
-}

instance Functor FoldLList where
    -- fmap f (FoldLList g) = FoldLList $ \h z -> g (\acc x -> h acc (f x)) z
    fmap f (FoldLList g) = FoldLList $ \h z -> g ((. f) . h) z

visibleTreeCoords :: Forest -> S.Set TreeIx
visibleTreeCoords forest =
    let ((rmin, cmin), (rmax, cmax)) = bounds forest
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
