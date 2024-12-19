module Control.Comonad
    ( Comonad(..)
    , liftW
    )
    where

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    duplicate = extend id
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
    {-# MINIMAL extract, (duplicate | extend) #-}

liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)
