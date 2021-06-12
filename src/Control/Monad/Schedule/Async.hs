{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Schedule.Async where

import Control.Monad.Schedule.Class as Class

newtype Async m a = Async { unAsync :: m a }
  deriving (Functor, Applicative, Monad)

data AsyncAwaitT m a where
  Pure :: a -> AsyncAwaitT m a
  Free :: m (AsyncAwaitT m a) -> AsyncAwaitT m a
  -- StartAsync :: Async m a -> AsyncAwaitT m a
  -- Await :: AsyncAwaitT m a -> AsyncAwaitT m a -- Not sure whether it's needed

instance Functor m => Functor (AsyncAwaitT m) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free a) = Free $ fmap f <$> a
  -- fmap f (StartAsync ma) = StartAsync $ f <$> ma
  -- fmap f (Await ma) = Await $ f <$> ma

instance (Monad m, MonadSchedule m) => Applicative (AsyncAwaitT m) where
  pure = Pure

  Pure f <*> a = f <$> a
  Free f <*> Pure a = Free $ fmap ($ a) <$> f
  -- Free f <*> Free a = Free $ fmap (<*>) f <*> a
  Free f <*> Free a = Free $ uncurry (<*>) <$> Class.async f a
  -- StartAsync (Async mf) <*> StartAsync (Async ma) = StartAsync $ Async $ uncurry ($) <$> Class.async mf ma
  -- Pure f <*> StartAsync ma = StartAsync $ f <$> ma
  -- Free f <*> StartAsync ma = Free $ _ <$> f
  -- StartAsync mf <*> Pure a = StartAsync $ ($ a) <$> mf
  -- StartAsync mf <*> Free a = Free $ _

instance (Monad m, MonadSchedule m) => Monad (AsyncAwaitT m) where
  Pure a >>= f = f a
  Free a >>= f = Free $ (>>= f) <$> a

-- https://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html
