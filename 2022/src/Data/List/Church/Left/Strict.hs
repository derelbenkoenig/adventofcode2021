module Data.List.Church.Left.Strict where

import Data.List (foldl')

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

