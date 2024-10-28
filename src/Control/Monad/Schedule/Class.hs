{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Only needed for Functor (SchedulingContext m)
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Schedule.Class where

-- base
import Control.Arrow
import Control.Concurrent
import Data.Either
import Data.Foldable (fold, forM_, traverse_)
import Data.Function
import Data.Functor.Const
import Data.Functor.Identity
import Data.Kind (Type)
import Data.List.NonEmpty hiding (length)
import Prelude hiding (map, zip)

-- base-compat
import Data.Functor.Compat (unzip)

-- transformers
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.CPS as CPSWriter
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad (liftM, ap)
import Control.Monad.IO.Class (MonadIO (..))

{- | 'Monad's in which actions can be scheduled concurrently.

@'schedule' actions@ is expected to run @actions@ concurrently,
whatever that means for a particular monad @m@.
'schedule' does not return before at least one value has finished,
and the returned values @'NonEmpty' a@ are all those that finish first.
The actions @[m a]@ (possibly empty) are the remaining, still running ones.
Executing any of them is expected to be blocking,
and awaits the return of the corresponding action.

A lawful instance is considered to preserve pure values.
Applying 'schedule' to values like @'pure' a@ will eventually return exactly these values.

'schedule' thus can be thought of as a concurrency-utilizing version of 'sequence'.
-}
class MonadSchedule m where
  type SchedulingContext m :: Type -> Type
  type SchedulingContext m = Const ()

  -- | Run the actions concurrently,
  --   and return the result of the first finishers,
  --   together with completions for the unfinished actions.
  schedule :: NonEmpty (m a) -> m (NonEmpty a, [m a])
  default schedule :: (Monad m) => NonEmpty (m a) -> m (NonEmpty a, [m a])
  schedule actions = createContext >>= flip scheduleWithContext actions

  -- FIXME Check haddocks that either schedule or createContext, scheduleWithContext are required

  createContext :: m (SchedulingContext m a)
  default createContext :: (Applicative m, SchedulingContext m ~ Const ()) => m (SchedulingContext m a)
  createContext = pure $ Const ()

  scheduleWithContext :: SchedulingContext m a -> NonEmpty (m a) -> m (NonEmpty a, [m a])
  default scheduleWithContext :: (SchedulingContext m ~ Const ()) => SchedulingContext m a -> NonEmpty (m a) -> m (NonEmpty a, [m a])
  scheduleWithContext _ = schedule
{- | Keeps 'schedule'ing actions until all are finished.
  Returns the same set of values as 'sequence',
  but utilises concurrency and may thus change the order of the values.
-}
scheduleAndFinish :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
scheduleAndFinish actions = do
  (finishedFirst, running) <- schedule actions
  case running of
    [] -> return finishedFirst
    (a : as) -> do
      finishedLater <- scheduleAndFinish $ a :| as
      return $ finishedFirst <> finishedLater

{- | Uses 'scheduleAndFinish' to execute all actions concurrently,
  then orders them again.
  Thus it behaves semantically like 'sequence',
  but leverages concurrency.
-}
sequenceScheduling :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
sequenceScheduling =
  zip [1 ..]
    >>> map strength
    >>> scheduleAndFinish
    >>> fmap (sortWith fst >>> map snd)
  where
    strength :: (Functor m) => (a, m b) -> m (a, b)
    strength (a, mb) = (a,) <$> mb

-- | When there are no effects, return all values immediately
instance MonadSchedule Identity where
  schedule as = (,[]) <$> sequence as

{- | Fork all actions concurrently in separate threads and wait for the first one to complete.

Many monadic actions complete at nondeterministic times
(such as event listeners),
and it is thus impossible to schedule them deterministically
with most other actions.
Using concurrency, they can still be scheduled with all other actions in 'IO',
by running them in separate GHC threads.

Caution: Using 'schedule' repeatedly on the returned continuations of a previous 'schedule' call
will add a layer of indirection to the continuation every time,
eventually slowing down performance and building up memory.
For a monad that doesn't have this problem, see 'Control.Monad.Schedule.FreeAsync.FreeAsyncT'.
-}
instance MonadSchedule IO where
  type SchedulingContext IO = MVar

  createContext = newEmptyMVar

  scheduleWithContext var as = do
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

data FunnyIO a = Boring (IO a) | Funny (MVar a)

instance Functor FunnyIO where
  fmap = liftM
instance Applicative FunnyIO where
  pure = Boring . pure
  (<*>) = ap

instance Monad FunnyIO where
  funny >>= f = Boring $ runFunnyIO funny >>= runFunnyIO . f

instance MonadIO FunnyIO where
  liftIO = Boring

runFunnyIO :: FunnyIO a -> IO a
runFunnyIO (Boring io) = io
runFunnyIO (Funny var) = takeMVar var

instance MonadSchedule FunnyIO where
  type SchedulingContext FunnyIO = MVar

  createContext = liftIO newEmptyMVar

  scheduleWithContext var as = do
    traverse_ (background var) as
    a <- liftIO $ takeMVar var
    as' <- liftIO $ drain var
    let remaining = replicate (length as - 1 - length as') $ liftIO $ takeMVar var
    return (a :| as', remaining)
    where
      peek :: FunnyIO a -> IO (Either (FunnyIO a) a)
      peek (Boring io) = pure $ Left $ Boring io
      peek (Funny var) = maybe (Left (Funny var)) Right <$> tryTakeMVar var

      background :: MVar a -> FunnyIO a -> FunnyIO (FunnyIO a)
      background var (Boring io) = do
        liftIO $ forkIO $ putMVar var =<< io
        return $ Funny var
      background var funny@(Funny var') = if var == var'
          then return funny
          else background var $ liftIO $ takeMVar var'

      drain :: MVar a -> IO [a]
      drain var = do
        aMaybe <- tryTakeMVar var
        case aMaybe of
          Just a -> do
            as' <- drain var
            return $ a : as'
          Nothing -> return []
-- | Pass through the scheduling functionality of the underlying monad
instance (Functor m, MonadSchedule m) => MonadSchedule (IdentityT m) where
  type SchedulingContext (IdentityT m) = SchedulingContext m

  createContext = IdentityT createContext

  scheduleWithContext context =
    fmap runIdentityT
      >>> scheduleWithContext context
      >>> fmap (fmap (fmap IdentityT))
      >>> IdentityT

  schedule =
    fmap runIdentityT
      >>> schedule
      >>> fmap (fmap (fmap IdentityT))
      >>> IdentityT

{- | Write in the order of scheduling:
  The first actions to return write first.
-}
instance (Monoid w, Functor m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (LazyWriter.WriterT w m) where
  type SchedulingContext (LazyWriter.WriterT w m) = SchedulingContext m

  createContext = LazyWriter.WriterT $ (,mempty) <$> createContext

  scheduleWithContext context =
    fmap LazyWriter.runWriterT
      >>> scheduleWithContext ((,mempty) <$> context)
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap LazyWriter.WriterT))
      >>> LazyWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)
  schedule =
    fmap LazyWriter.runWriterT
      >>> schedule
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap LazyWriter.WriterT))
      >>> LazyWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

{- | Write in the order of scheduling:
  The first actions to return write first.
-}
instance (Monoid w, Functor m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (StrictWriter.WriterT w m) where
  type SchedulingContext (StrictWriter.WriterT w m) = SchedulingContext m

  createContext = StrictWriter.WriterT $ (,mempty) <$> createContext

  scheduleWithContext context =
    fmap StrictWriter.runWriterT
      >>> scheduleWithContext ((,mempty) <$> context)
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap StrictWriter.WriterT))
      >>> StrictWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

  schedule =
    fmap StrictWriter.runWriterT
      >>> schedule
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap StrictWriter.WriterT))
      >>> StrictWriter.WriterT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

{- | Write in the order of scheduling:
  The first actions to return write first.
-}
instance (Monoid w, Functor m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (CPSWriter.WriterT w m) where
  type SchedulingContext (CPSWriter.WriterT w m) = SchedulingContext m

  createContext = CPSWriter.writerT $ (,mempty) <$> createContext

  scheduleWithContext context =
    fmap CPSWriter.runWriterT
      >>> scheduleWithContext ((,mempty) <$> context)
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap CPSWriter.writerT))
      >>> CPSWriter.writerT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

  schedule =
    fmap CPSWriter.runWriterT
      >>> schedule
      >>> fmap (first (fmap fst &&& (fmap snd >>> fold)) >>> assoc >>> first (second $ fmap CPSWriter.writerT))
      >>> CPSWriter.writerT
    where
      assoc :: ((a, w), c) -> ((a, c), w)
      assoc ((a, w), c) = ((a, c), w)

-- FIXME Relax Monad type class

{- | Broadcast the same environment to all actions.
  The continuations keep this initial environment.
-}
instance (Monad m, MonadSchedule m) => MonadSchedule (ReaderT r m) where
  schedule actions = ReaderT $ \r ->
    fmap (`runReaderT` r) actions
      & schedule
      & fmap (second $ fmap lift)

{- | Combination of 'WriterT' and 'ReaderT'.
  Pass the same initial environment to all actions
  and write to the log in the order of scheduling in @m@.
-}
instance (Monoid w, Functor m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (AccumT w m) where
  type SchedulingContext (AccumT w m) = SchedulingContext m

  -- FIXME It's also a bit weird I have to do this dance everywhere just because lift requires a monad
  createContext = AccumT $ const $ (,mempty) <$> createContext

  schedule actions = AccumT $ \w ->
    fmap (`runAccumT` w) actions
      & schedule
      & fmap collectWritesAndWrap
    where
      collectWritesAndWrap ::
        (Monoid w) =>
        (NonEmpty (a, w), [m (a, w)]) ->
        ((NonEmpty a, [AccumT w m a]), w)
      collectWritesAndWrap (finished, running) =
        let (as, logs) = Data.Functor.Compat.unzip finished
         in ((as, AccumT . const <$> running), fold logs)

  scheduleWithContext context actions = AccumT $ \w ->
    fmap (`runAccumT` w) actions
      & scheduleWithContext ((,w) <$> context) -- FIXME this is a bit iffy. mempty better?
      & fmap collectWritesAndWrap
    where
      collectWritesAndWrap ::
        (Monoid w) =>
        (NonEmpty (a, w), [m (a, w)]) ->
        ((NonEmpty a, [AccumT w m a]), w)
      collectWritesAndWrap (finished, running) =
        let (as, logs) = Data.Functor.Compat.unzip finished
         in ((as, AccumT . const <$> running), fold logs)

{- | Schedule all actions according to @m@ and in case of exceptions
  throw the first exception of the immediately returning actions.
-}
instance (Applicative m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (ExceptT e m) where
  type SchedulingContext (ExceptT e m) = SchedulingContext m

  createContext = ExceptT $ Right <$> createContext

  schedule =
    fmap runExceptT
      >>> schedule
      >>> fmap (sequenceA *** fmap ExceptT >>> extrudeEither)
      >>> ExceptT
    where
      extrudeEither :: (Either e a, b) -> Either e (a, b)
      extrudeEither (ea, b) = (,b) <$> ea

  scheduleWithContext context =
    fmap runExceptT
      >>> scheduleWithContext (Right <$> context)
      >>> fmap (sequenceA *** fmap ExceptT >>> extrudeEither)
      >>> ExceptT
    where
      extrudeEither :: (Either e a, b) -> Either e (a, b)
      extrudeEither (ea, b) = (,b) <$> ea

instance (Applicative m, MonadSchedule m, Functor (SchedulingContext m)) => MonadSchedule (MaybeT m) where
  type SchedulingContext (MaybeT m) = SchedulingContext m

  createContext = MaybeT $ Just <$> createContext

  scheduleWithContext context =
    fmap (maybeToExceptT ())
      >>> scheduleWithContext context
      >>> exceptToMaybeT
      >>> fmap (second $ fmap exceptToMaybeT)

  -- FIXME this duplication is a bit annoying
  schedule =
    fmap (maybeToExceptT ())
      >>> schedule
      >>> exceptToMaybeT
      >>> fmap (second $ fmap exceptToMaybeT)

-- instance (Monad m, MonadSchedule m) => MonadSchedule (ContT r m) where
--   schedule actions = ContT $ \scheduler
--     -> fmap (runContT >>> _) actions
--     & schedule
--     & _

-- FIXME Try this instead
-- instance (Monad m, Someclass r) => MonadSchedule (ContT r m) where

{- | Runs two values in a 'MonadSchedule' concurrently
  and returns the first one that yields a value
  and a continuation for the other value.
-}
race ::
  (Applicative m, MonadSchedule m) =>
  m a ->
  m b ->
  m (Either (a, m b) (m a, b))
race aM bM = recoverResult <$> schedule ((Left <$> aM) :| [Right <$> bM])
  where
    recoverResult :: (Applicative m) => (NonEmpty (Either a b), [m (Either a b)]) -> Either (a, m b) (m a, b)
    recoverResult (Left a :| [], [bM']) = Left (a, fromRight e <$> bM')
    recoverResult (Right b :| [], [aM']) = Right (fromLeft e <$> aM', b)
    recoverResult (Left a :| [Right b], []) = Left (a, pure b)
    recoverResult (Right b :| [Left a], []) = Right (pure a, b)
    recoverResult _ = e
    e = error "race: Internal error"

-- FIXME I should only need Selective

-- | Runs both schedules concurrently and returns their results at the end.
async ::
  (Monad m, MonadSchedule m) =>
  m a ->
  m b ->
  m (a, b)
async aSched bSched = do
  ab <- race aSched bSched
  case ab of
    Left (a, bCont) -> do
      b <- bCont
      return (a, b)
    Right (aCont, b) -> do
      a <- aCont
      return (a, b)

{- | Run both actions concurrently and apply the first result to the second.

Use as a concurrent replacement for '<*>' from 'Applicative'.
-}
apSchedule :: (MonadSchedule m, Monad m) => m (a -> b) -> m a -> m b
apSchedule f a = uncurry id <$> async f a

-- | Concurrent replacement for '*>' from 'Applicative' or '>>' from 'Monad'.
scheduleWith :: (MonadSchedule m, Monad m) => m a -> m b -> m b
scheduleWith a b = (id <$ a) `apSchedule` b
