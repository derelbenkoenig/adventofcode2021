module Control.Monad.List.Trans
    ( ListT(..)
    , List
    , runListT
    , runList
    , foldrListT
    , hoistList
    , takeListT
    , runListT_
    , iterateListT
    , takeWhileListT
    ) where

import Data.Bifunctor
import Data.Functor.Identity
import Control.Applicative
import Control.Monad (ap, MonadPlus)
import Control.Monad.Trans.Class

newtype ListT m a = ListT { stepListT :: m (Maybe (a, ListT m a)) }

instance Functor m => Functor (ListT m) where
    fmap g (ListT f) = ListT $ (fmap . fmap) (bimap g (fmap g)) f

instance Monad m => Alternative (ListT m) where
    empty = ListT $ pure Nothing
    ListT xs <|> ListT ys = ListT $ do
        ma <- xs
        case ma of
          Just (a, as) -> pure $ Just $ (a, as <|> ListT ys)
          Nothing -> ys

instance Monad m => Semigroup (ListT m a) where
    (<>) = (<|>)

instance Monad m => Monoid (ListT m a) where
    mempty = empty

instance Monad m => Applicative (ListT m) where
    pure a = ListT $ pure $ Just (a, empty)
    (<*>) = ap

instance Monad m => Monad (ListT m) where
    ListT ma >>= f = ListT $ do
        maybeList <- ma
        case maybeList of
          Nothing -> pure Nothing
          Just (a, as) -> stepListT $ f a <|> (as >>= f)

instance Monad m => MonadPlus (ListT m) where

instance MonadTrans ListT where
    lift = ListT . fmap (Just . (, empty))

foldrListT :: Monad m => (a -> b -> b) -> b -> ListT m a -> m b
foldrListT f z (ListT m) = do
    ma <- m
    case ma of
      Just (a, as) -> f a <$> foldrListT f z as
      Nothing -> pure z

runListT :: Monad m => ListT m a -> m [a]
runListT = foldrListT (:) []

runListT_ :: Monad m => ListT m a -> m ()
runListT_ = (() <$) . runListT

type List a = ListT Identity a

runList :: List a -> [a]
runList = runIdentity . runListT

hoistList :: Alternative m => [a] -> m a
hoistList = foldr ((<|>) . pure) empty

takeListT :: Monad m => Int -> ListT m a -> ListT m a
takeListT 0 = const empty
takeListT n = ListT . (fmap . fmap) (second (takeListT (n - 1))) . stepListT

takeWhileListT :: Monad m => (a -> Bool) -> ListT m a -> ListT m a
takeWhileListT p (ListT m) = ListT $ do
    ma <- m
    case ma of
      Nothing -> pure Nothing
      Just (a, as) | p a -> stepListT $ pure a <|> takeWhileListT p as
                   | otherwise -> pure Nothing

iterateListT :: Monad m => (a -> a) -> a -> ListT m a
iterateListT f x = pure x <|> iterateListT f (f x)
