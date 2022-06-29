{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Can where

-- base
import Data.List.NonEmpty

-- free
import Control.Monad.Trans.Free

-- monad-schedule
import Control.Monad.Schedule.Class
import Control.Category ((>>>))
import Control.Arrow (first, second)
import Data.Either (partitionEithers)
import Control.Monad.Trans.Class

newtype CanT t m a = CanT
  { runCanT :: FreeT (t m) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans t => MonadTrans (CanT t) where
  lift = CanT . lift

-- FIXME doesn't this work for a general functor?
-- Problem is only that this overlaps with ScheduleT => newtype that one
instance (Monad m, MonadSchedule m, MonadTrans t, Functor (t m)) => MonadSchedule (CanT t m) where
  schedule = fmap runCanT
    >>> step
    >>> fmap (second $ fmap CanT)
    >>> CanT
    where
      step :: (Monad m, MonadSchedule m, MonadTrans t, Functor (t m)) => NonEmpty (FreeT (t m) m a)
         -> FreeT (t m) m (NonEmpty a, [FreeT (t m) m a])
      step actions = do
        (finished, running) <- lift $ schedule $ runFreeT <$> actions
        let (pures, frees) = partitionEithers $ freeToEither <$> toList finished
            running' = FreeT <$> running
        case nonEmpty pures of
          Nothing -> do
            freesOneStep <- sequence $ liftF <$> frees
            step $ fromList $ running' ++ freesOneStep
          Just pures -> return (pures, running' ++ (FreeT . return . Free <$> frees))

      freeToEither :: FreeF f a b -> Either a (f b)
      freeToEither (Pure a) = Left a
      freeToEither (Free fb) = Right fb
