{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Monad.Schedule.Class where


-- base
import Control.Arrow
import Control.Concurrent
import Data.Either
import Data.Foldable (fold, forM_)
import Data.List.NonEmpty hiding (length)
import Data.Function
import Data.Kind (Type)
import Data.Void

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.Trans.Cont
import Control.Monad (void)
import Unsafe.Coerce (unsafeCoerce)
import Data.Functor.Identity
import Data.Maybe (fromJust)
import Prelude hiding (map, zip)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

{- | 'Monad's in which actions can be scheduled concurrently.

@'schedule' actions@ is expected to run @actions@ concurrently,
whatever that means for a particular monad @m@.
'schedule' does not return before at least one value has finished,
and the returned values @'NonEmpty' a@ are all those that finish first.
The actions @[m a]@ (possibly empty) are the remaining, still running ones.
Executing any of them is expected to be blocking,
and awaits the return of the corresponding action.

A lawful instance is considered to satisfy these conditions:

  * The set of returned values is invariant under scheduling.
    In other words, @sequence@ will result in the same set of values as @scheduleAndFinish@.
'schedule' thus can be thought of as a concurrency-utilizing version of 'sequence'.
-}
class MonadSchedule m where
  -- | Run the actions concurrently,
  --   and return the result of the first finishers,
  --   together with completions for the unfinished actions.
  schedule :: NonEmpty (m a) -> m (NonEmpty a, [m a])

-- | Keeps 'schedule'ing actions until all are finished.
--   Returns the same set of values as 'sequence',
--   but utilises concurrency and may thus change the order of the values.
scheduleAndFinish :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
scheduleAndFinish actions = do
  (finishedFirst, running) <- schedule actions
  case running of
    [] -> return finishedFirst
    (a : as) -> do
      finishedLater <- scheduleAndFinish $ a :| as
      return $ finishedFirst <> finishedLater

-- | Uses 'scheduleAndFinish' to execute all actions concurrently,
--   then orders them again.
--   Thus it behaves semantically like 'sequence',
--   but leverages concurrency.
sequenceScheduling :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
sequenceScheduling
  =   zip [1..]
  >>> map strength
  >>> scheduleAndFinish
  >>> fmap (sortWith fst >>> map snd)
  where
    strength :: Functor m => (a, m b) -> m (a, b)
    strength (a, mb) = (a, ) <$> mb

{- |
Fork all actions concurrently in separate threads and wait for the first one to complete.

Many monadic actions complete at nondeterministic times
(such as event listeners),
and it is thus impossible to schedule them deterministically
with most other actions.
Using concurrency, they can still be scheduled with all other actions in 'IO',
by running them in separate GHC threads.
-}
instance MonadSchedule IO where
  schedule as = do
    var <- newEmptyMVar
    forM_ as $ \action -> forkIO $ putMVar var =<< action
    a <- takeMVar var
    as' <- drain var
    let remaining = replicate (length as - 1 - length as') $ takeMVar var
    return (a :| as', remaining)
      where
        drain :: MVar a -> IO [a]
        drain var = do
          aMaybe <- tryTakeMVar var
          case aMaybe of
            Just a -> do
              as' <- drain var
              return $ a : as'
            Nothing -> return []

-- TODO Needs dependency
-- instance MonadSchedule STM where

-- | Write in the order of scheduling:
--   The first actions to return write first.
instance (Monoid w, Functor m, MonadSchedule m) => MonadSchedule (WriterT w m) where
  schedule = fmap runWriterT
    >>> schedule
    >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap WriterT))
    >>> WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

-- | Broadcast the same environment to all actions.
--   The continuations keep this initial environment.
instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule actions = ReaderT $ \r
    -> fmap (`runReaderT` r) actions
    & schedule
    & fmap (second $ fmap lift)

-- | Combination of 'WriterT' and 'ReaderT'.
--   Pass the same initial environment to all actions
--   and write to the log in the order of scheduling in @m@.
instance (Monoid w, Monad m, MonadSchedule m) => MonadSchedule (AccumT w m) where
  schedule actions = AccumT $ \w
    -> fmap (`runAccumT` w) actions
    & schedule
    & fmap collectWritesAndWrap
    where
      collectWritesAndWrap ::
        Monoid w =>
        (NonEmpty (a, w), [m (a, w)]) ->
        ((NonEmpty a, [AccumT w m a]), w)
      collectWritesAndWrap (finished, running) =
        let (as, logs) = NonEmpty.unzip finished
        in ((as, AccumT . const <$> running), fold logs)

-- | Schedule all actions according to @m@ and in case of exceptions
--   throw the first exception of the immediately returning actions.
instance (Monad m, MonadSchedule m) => MonadSchedule (ExceptT e m) where
  schedule
    =   fmap runExceptT
    >>> schedule
    >>> fmap ((sequenceA *** fmap ExceptT) >>> extrudeEither)
    >>> ExceptT
    where
      extrudeEither :: (Either e a, b) -> Either e (a, b)
      extrudeEither (ea, b) = (, b) <$> ea

instance (Monad m, MonadSchedule m) => MonadSchedule (MaybeT m) where
  schedule
    =   fmap (maybeToExceptT ())
    >>> schedule
    >>> exceptToMaybeT
    >>> fmap (second $ fmap exceptToMaybeT)

-- instance (Monad m, MonadSchedule m) => MonadSchedule (ContT r m) where
--   schedule actions = ContT $ \scheduler
--     -> fmap (runContT >>> _) actions
--     & schedule
--     & _

-- | Runs two values in a 'MonadSchedule' concurrently
--   and returns the first one that yields a value
--   and a continuation for the other value.
race
  :: (Monad m, MonadSchedule m)
  => m a -> m b
  -> m (Either (a, m b) (m a, b))
race aM bM = recoverResult <$> schedule ((Left <$> aM) :| [Right <$> bM])
  where
    recoverResult :: Monad m => (NonEmpty (Either a b), [m (Either a b)]) -> Either (a, m b) (m a, b)
    recoverResult (Left a :| [], [bM']) = Left (a, fromRight e <$> bM')
    recoverResult (Right b :| [], [aM']) = Right (fromLeft e <$> aM', b)
    recoverResult (Left a :| [Right b], []) = Left (a, return b)
    recoverResult (Right b :| [Left a], []) = Right (return a, b)
    recoverResult _ = e
    e = error "race: Internal error"

-- FIXME I should only need Selective
-- | Runs both schedules concurrently and returns their results at the end.
async
  :: (Monad m, MonadSchedule m)
  => m  a -> m b
  -> m (a,     b)
async aSched bSched = do
  ab <- race aSched bSched
  case ab of
    Left  (a, bCont) -> do
      b <- bCont
      return (a, b)
    Right (aCont, b) -> do
      a <- aCont
      return (a, b)
