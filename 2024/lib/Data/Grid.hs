{-# LANGUAGE RecordWildCards #-}

module Data.Grid
    ( Grid(..)
    , grid
    , pos
    , seek
    , peek
    , experiment
    , inBounds
    )
    where

import Control.Comonad
import Data.Array.IArray

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

pos :: Grid a -> (Int, Int)
pos = focus

seek :: (Int, Int) -> Grid a -> Grid a
seek ix g = g{focus=ix}

experiment :: Functor f => ((Int, Int) -> f (Int, Int)) -> Grid a -> f a
experiment getNeighbors Grid{..} = fmap (cells !) $ getNeighbors focus

grid :: ((Int, Int), (Int, Int)) -> [((Int, Int), a)] -> Grid a
grid = (fromArray .) . array

inBounds :: Grid a -> (Int, Int) -> Bool
inBounds Grid{..} ix = inRange (bounds cells) ix
