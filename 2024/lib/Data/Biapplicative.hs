module Data.Biapplicative 
    ( module X
    , Biapplicative(..)
    )
    where

import Data.Bifunctor as X

class Bifunctor p => Biapplicative p where
    {-# MINIMAL bipure, ( (<<*>>) | biLiftA2) #-}
    bipure :: a -> b -> p a b

    (<<*>>) :: p (a -> a') (b -> b') -> p a b -> p a' b'
    (<<*>>) = biLiftA2 id id

    biLiftA2 :: (a -> a' -> a'') -> (b -> b' -> b'') -> p a b -> p a' b' -> p a'' b''
    biLiftA2 f g x y = bimap f g x <<*>> y
    
    (*>>) :: p a a' -> p b b' -> p b b'
    x *>> y = biLiftA2 (const id) (const id) x y

    (<<*) :: p a a' -> p b b' -> p a a'
    x <<* y = biLiftA2 const const x y

instance Biapplicative (,) where
    bipure = (,)

    (f, g) <<*>> (x, y) = (f x, g y)

    biLiftA2 f g (x, x') (y, y') = (f x y, g x' y')
