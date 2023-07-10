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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Control.Monad.Schedule.Class where


-- base
import Control.Arrow
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Either
import Data.Foldable (fold, forM_)
import Data.Function
import Data.Functor.Identity
import Data.Kind (Type)
import Data.List.NonEmpty hiding (uncons, length)
import Data.Maybe (fromJust)
import Data.Void
import Prelude hiding (map, zip)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.List.NonEmpty as NonEmpty hiding (uncons)

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Data.Vector.Sized.Trans (mapM, VectorT (..), uncons, runVectorT, replicateM, cons, singleton, VectorT' (..))
import qualified Data.Vector.Sized.Trans.Some as Some
import Data.Vector.Sized.Trans.Some (SomeVectorT)
import GHC.TypeNats
import Control.Monad.Morph
import ListT (ListT)
import ListT qualified

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
-- FIXME rather constrained class method?
class Monad m => MonadSchedule m where
  -- | Run the actions concurrently,
  --   and return the result of the first finishers,
  --   together with completions for the unfinished actions.
  schedule :: NonEmpty (m a) -> m (NonEmpty a, [m a])
  schedule'' :: Vector (n + 1) (m a) -> m (forall i j . (i + j ~ n) (Vector (i + 1) a, Vector j (m a)))
  schedule''' :: Vector (n + 1) (m a) -> m ((a, Vector n (m a)))

  schedule' :: KnownNat n => VectorT n Identity (m a) -> VectorT n m a

-- | Keeps 'schedule'ing actions until all are finished.
--   Returns the same set of values as 'sequence',
--   but utilises concurrency and may thus change the order of the values.
scheduleAndFinish :: (Monad m, MonadSchedule m, KnownNat n) => VectorT n Identity (m a) -> m (VectorT n Identity a)
scheduleAndFinish = runVectorT . schedule'

-- FIXME
{-
-- | Uses 'scheduleAndFinish' to execute all actions concurrently,
--   then orders them again.
--   Thus it behaves semantically like 'sequence',
--   but leverages concurrency.
sequenceScheduling :: (Monad m, MonadSchedule m) => VectorT n Identity (m a) -> m (VectorT n Identity a)
sequenceScheduling
  =   zip [1..]
  >>> map strength
  >>> scheduleAndFinish
  >>> fmap (sortWith fst >>> map snd)
  where
    strength :: Functor m => (a, m b) -> m (a, b)
    strength (a, mb) = (a, ) <$> mb
-}
-- | When there are no effects, return all values immediately
instance MonadSchedule Identity where
  schedule as = ( , []) <$> sequence as
  schedule' = fmap runIdentity

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
  schedule' VectorT { getVectorT = ioas } = VectorT $ do
    var <- newEmptyMVar
    forM_ (runIdentity ioas) $ \action -> forkIO $ putMVar var =<< action
    getVectorT $ replicateM $ takeMVar var

-- TODO Needs dependency
-- instance MonadSchedule STM where

-- | Pass through the scheduling functionality of the underlying monad
instance (Functor m, MonadSchedule m) => MonadSchedule (IdentityT m) where
  schedule
    =   fmap runIdentityT
    >>> schedule
    >>> fmap (fmap (fmap IdentityT))
    >>> IdentityT

  schedule'
    = fmap runIdentityT
    >>> schedule'
    >>> hoist IdentityT

-- | Write in the order of scheduling:
--   The first actions to return write first.
instance (Monoid w, Functor m, MonadSchedule m) => MonadSchedule (LazyWriter.WriterT w m) where
  schedule = fmap LazyWriter.runWriterT
    >>> schedule
    >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap LazyWriter.WriterT))
    >>> LazyWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

  schedule' = fmap LazyWriter.runWriterT
    >>> schedule'
    >>> hoist lift
    >>> Data.Vector.Sized.Trans.mapM LazyWriter.writer

-- | Write in the order of scheduling:
--   The first actions to return write first.
instance (Monoid w, Functor m, MonadSchedule m) => MonadSchedule (StrictWriter.WriterT w m) where
  schedule = fmap StrictWriter.runWriterT
    >>> schedule
    >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap StrictWriter.WriterT))
    >>> StrictWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)
  schedule' = fmap StrictWriter.runWriterT
    >>> schedule'
    >>> hoist lift
    >>> Data.Vector.Sized.Trans.mapM StrictWriter.writer

-- | Write in the order of scheduling:
--   The first actions to return write first.
instance (Monoid w, Functor m, MonadSchedule m) => MonadSchedule (CPSWriter.WriterT w m) where
  schedule = fmap CPSWriter.runWriterT
    >>> schedule
    >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap CPSWriter.writerT))
    >>> CPSWriter.writerT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)
  schedule' = fmap CPSWriter.runWriterT
    >>> schedule'
    >>> hoist lift
    >>> Data.Vector.Sized.Trans.mapM CPSWriter.writer

-- | Broadcast the same environment to all actions.
--   The continuations keep this initial environment.
instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule actions = ReaderT $ \r
    -> fmap (`runReaderT` r) actions
    & schedule
    & fmap (second $ fmap lift)

  schedule' actions
    = VectorT $ ReaderT $ \r
        -> fmap (`runReaderT` r) actions
        & schedule'
        & getVectorT
        & fmap (hoist lift)

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
  schedule' actions = VectorT $ AccumT $ \w
    -> fmap (`runAccumT` w) actions
    & schedule'
    & getVectorT
    & fmap getFirstLog
   where
    getFirstLog :: VectorT' n m (a, w) -> (VectorT' n (AccumT w m) a, w)
    getFirstLog VNil = (VNil, mempty)
    getFirstLog (VCons (a, w) aws) = (VCons a $ Data.Vector.Sized.Trans.mapM (accum . const) $ hoist lift aws, w)

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
  schedule'
    =   fmap runExceptT
    >>> schedule'
    >>> hoist lift
    >>> Data.Vector.Sized.Trans.mapM except

instance (Monad m, MonadSchedule m) => MonadSchedule (MaybeT m) where
  schedule
    =   fmap (maybeToExceptT ())
    >>> schedule
    >>> exceptToMaybeT
    >>> fmap (second $ fmap exceptToMaybeT)

  schedule'
    =   fmap (maybeToExceptT ())
    >>> schedule'
    >>> hoist exceptToMaybeT

instance MonadSchedule m => MonadSchedule (ListT m) where
  schedule'
    =   fmap ListT.uncons
    >>> schedule'
    >>> _

{-
instance (Monad m, MonadSchedule m) => MonadSchedule (ContT r m) where
  -- schedule actions = ContT $ \scheduler
  --   -> fmap (runContT >>> _) actions
  --   & schedule
  --   & _
  schedule' actions = VectorT $ ContT $ \cont
    -> fmap (runContT) actions
    & _
-}
newtype MyContT r m a = MyContT { getMyContT :: (a -> m r) -> m r }

instance Functor (MyContT r m) where
  fmap f MyContT {getMyContT} = MyContT $ \cont -> getMyContT $ cont . f

instance Applicative (MyContT r m) where
  pure a = MyContT $ \cont -> cont a
  MyContT f <*> MyContT a = MyContT $ \cont -> f $ \f -> a $ \a -> cont $ f a

instance Monad (MyContT r m) where
  MyContT ma >>= f = MyContT $ \cont -> ma $ \a -> getMyContT (f a) $ \b -> cont b

-- | Runs two values in a 'MonadSchedule' concurrently
--   and returns the first one that yields a value
--   and a continuation for the other value.
race
  :: (Monad m, MonadSchedule m)
  => m a -> m b
  -> m (Either (a, m b) (m a, b))
race aM bM = fmap (recoverResult . second (fmap fst . uncons)) $ uncons $ schedule' (Identity (Left <$> aM) `Data.Vector.Sized.Trans.cons` Data.Vector.Sized.Trans.singleton (Identity $ Right <$> bM))
  where
    recoverResult :: Functor m => (Either a b, m (Either a b)) -> Either (a, m b) (m a, b)
    recoverResult (Left a, mab) = Left (a, fromRight e <$> mab)
    recoverResult (Right b, mab) = Right (fromLeft e <$> mab, b)
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
