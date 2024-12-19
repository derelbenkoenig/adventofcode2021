{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Data.Zipper
    ( Zipper(..)
    , fromList
    , toList
    , toTuple
    -- ^ just for debugging purposes as it's easier to read the 'Show' output
    , reverseZipper
    , seekForward
    , seekBackward
    , dropFocus
    ) where

import Control.Comonad

data Zipper a = Zipper { zipped :: [a], focus :: a, unzipped :: [a] }
    deriving (Eq, Show)

instance Ord a => Ord (Zipper a) where
    compare (Zipper l1 f1 r1) (Zipper l2 f2 r2) =
        compare (reverse l1, f1, r1) (reverse l2, f2, r2)

reverseZipper :: Zipper a -> Zipper a
reverseZipper (Zipper ls a rs) = Zipper rs a ls

toTuple :: Zipper a -> ([a], a, [a])
toTuple Zipper{..} = (zipped, focus, unzipped)

instance Foldable Zipper where
    foldr f z Zipper{..} = foldl (flip f) (foldr f z (focus:unzipped)) zipped

instance Functor Zipper where
    fmap f Zipper{..} = Zipper (fmap f zipped) (f focus) (fmap f unzipped)

instance Comonad Zipper where
    extract = focus
    duplicate z@(Zipper ls a rs) =
        Zipper (dupZipper rs a ls reverseZipper) z (dupZipper ls a rs id)
            where
                dupZipper :: [a] -> a -> [a] -> (Zipper a -> Zipper a) -> [Zipper a]
                dupZipper _ _ [] _ = []
                dupZipper ls a (r:rs) k = k (Zipper (a:ls) r rs) : (dupZipper (a:ls) r rs k)

seekForward :: Zipper a -> Zipper a
seekForward Zipper{..} =
    case unzipped of
      (a:as) -> Zipper (focus:zipped) a as
      [] -> error "seekForward: Zipper out of bounds"

seekBackward :: Zipper a -> Zipper a
seekBackward Zipper{..} =
    case zipped of
      (a:as) -> Zipper as a (focus:zipped)
      [] -> error "seekBackward: Zipper out of bounds"

fromList :: [a] -> Zipper a
fromList [] = error "fromList: empty Zipper"
fromList (a:as) = Zipper [] a as

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

-- | convert to a list omitting the current focus
dropFocus :: Zipper a -> [a]
dropFocus Zipper{..} = reverse zipped ++ unzipped
