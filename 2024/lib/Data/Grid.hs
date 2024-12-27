{-# LANGUAGE RecordWildCards #-}

module Data.Grid
    ( Grid(..)
    , grid
    , pos
    , seek
    , peek
    , experiment
    , inBounds
    , find
    , trySeekBy
    , trySeek
    , tryPeek
    , edit
    , set
    , editM
    )
    where

import Control.Comonad
import Data.Array.IArray
import qualified Data.Foldable as F (find)

data Grid a = Grid
    { focus :: (Int, Int)
    , cells :: (Array (Int, Int) a)
    }
    deriving (Show, Eq)

instance Functor Grid where
    fmap f g@Grid{..} = g{cells = fmap f cells}

instance Foldable Grid where
    foldr f z Grid{..} = foldr f z cells

instance Comonad Grid where
    extract Grid{..} = cells ! focus
    extend f g@Grid{..} = g{cells = genArray (bounds cells) (\ix -> f g{focus = ix})}
    duplicate g@Grid{..} = Grid focus $ genArray (bounds cells) (\ix -> g{focus = ix})

fromArray :: Array (Int, Int) a -> Grid a
fromArray arr = Grid (fst $ bounds arr) arr

peek :: (Int, Int) -> Grid a -> a
peek ix Grid{..} = cells ! ix

tryPeek :: (Int, Int) -> Grid a -> Maybe a
tryPeek ix g =
    if inBounds g ix
       then Just $ peek ix g
       else Nothing

pos :: Grid a -> (Int, Int)
pos = focus

seek :: (Int, Int) -> Grid a -> Grid a
seek ix g =
    if inBounds g ix
       then g{focus=ix}
       else error "seek: out of grid bounds"

trySeek :: (Int, Int) -> Grid a -> Maybe (Grid a)
trySeek ix g =
    if inBounds g ix
       then Just g{focus=ix}
       else Nothing

experiment :: Functor f => ((Int, Int) -> f (Int, Int)) -> Grid a -> f a
experiment getNeighbors Grid{..} = fmap (cells !) $ getNeighbors focus

grid :: ((Int, Int), (Int, Int)) -> [((Int, Int), a)] -> Grid a
grid = (fromArray .) . array

inBounds :: Grid a -> (Int, Int) -> Bool
inBounds Grid{..} ix = inRange (bounds cells) ix

find :: (a -> Bool) -> Grid a -> Maybe (Int, Int)
find f = fmap fst . F.find (f . snd) . assocs . cells

trySeekBy :: (a -> Bool) -> Grid a -> Maybe (Grid a)
trySeekBy p g = fmap (flip seek g) (find p g)

edit :: (a -> a) -> Grid a -> Grid a
edit f g@Grid{..} = g{cells = cells // [(focus, f (extract g))]}

editM :: Functor m => (a -> m a) -> Grid a -> m (Grid a)
editM f g = flip set g <$> f (extract g)

set :: a -> Grid a -> Grid a
set x g@Grid{..} = g{cells = cells // [(focus, x)]}
