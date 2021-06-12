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

-- hlist
-- import Data.HList

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
    In other words, @sequence@ will result in the same set oq values as @scheduleAndFinish@.
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

data HMList m (types :: [Type]) where
  Nil :: HMList m '[]
  (:.) :: (CollectEithersToList ts, DistributeListToEithers ts) => m t -> HMList m ts -> HMList m (t ': ts)

{-
type family ListToEithers (types :: [Type]) :: Type where
  ListToEithers '[] = Void
  ListToEithers (t ': ts) = Either t (ListToEithers ts)

class DistributeListToEithers (types :: [Type]) where
  distribute :: List types -> [ListToEithers types]

instance DistributeListToEithers '[] where
  distribute Nil = []

instance DistributeListToEithers ts => DistributeListToEithers (t ': ts) where
  distribute (t :. ts) = Left t : (Right <$> distribute ts)

class CollectEithersToList (types :: [Type]) where
  collect :: [ListToEithers types] -> Maybe (List types)

instance CollectEithersToList '[] where
  collect [] = Just Nil
  collect _ = Nothing

instance CollectEithersToList ts => CollectEithersToList (t ': ts) where
  collect ts' = do
    ([Left t], tsRight) <- pure $ partitionEithers ts'
    ts <- sequence $ rightToMaybe <$> tsRight
    tsCollected <- collect ts
    return $ t :. tsCollected

asyncList :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
asyncList mas = do
  (as, mas') <- schedule mas
  case as' of
    -- All actions returned immediately
    [] -> return as
    -- All but one actions returned...
    [ma] -> do
      -- ...so just await the remaining action
      a' <- ma
      return $ as <> pure a'
    -- Several actions didn't return yet, schedule them again repeatedly
    _ -> do
      as' <- asyncList mas
      return $ as <> as'

-- TODO: PR to hlist
type family Map m ts where
  Map m '[] = '[]
  Map m (t ': ts) = m t ': Map m ts

asyncHList :: (Monad m, MonadSchedule m) => List (Map m ts) -> m (List ts)
asyncHList Nil = return Nil
asyncHList ts@(_ :. _) = _ <$> asyncList (fromList $ distribute ts)
-- asyncHList ts@(_ :. _) = collect . fromJust . toList <$> asyncList (fromList $ distribute ts)
-}

type family ListToEithers (types :: [Type]) :: Type where
  ListToEithers '[] = Void
  ListToEithers (t ': ts) = Either t (ListToEithers ts)

class DistributeListToEithers (types :: [Type]) where
  distribute :: Functor m => HMList m types -> [m (ListToEithers types)]

instance DistributeListToEithers '[] where
  distribute Nil = []

instance DistributeListToEithers ts => DistributeListToEithers (t ': ts) where
  distribute (t :. ts) = (Left <$> t) : (fmap Right <$> distribute ts)

class CollectEithersToList (types :: [Type]) where
  collect :: [ListToEithers types] -> Maybe (HMList Identity types)

instance CollectEithersToList '[] where
  collect [] = Just Nil
  collect _ = Nothing

instance CollectEithersToList ts => CollectEithersToList (t ': ts) where
  collect ts' = do
    ([t], ts) <- pure $ partitionEithers ts'
    tsCollected <- collect ts
    return $ Identity t :. tsCollected

-- TODO: I only need selective here
asyncList :: (Monad m, MonadSchedule m) => NonEmpty (m a) -> m (NonEmpty a)
asyncList mas = do
  (as, mas') <- schedule mas
  case mas' of
    -- All actions returned immediately
    [] -> return as
    -- All but one actions returned...
    [ma] -> do
      -- ...so just await the remaining action
      a' <- ma
      return $ as <> pure a'
    -- Several actions didn't return yet, schedule them again repeatedly
    _ -> do
      as' <- asyncList mas
      return $ as <> as'

-- TODO: PR to hlist
type family Map m ts where
  Map m '[] = '[]
  Map m (t ': ts) = m t ': Map m ts

asyncHList :: forall m ts . (Monad m, MonadSchedule m, CollectEithersToList ts, DistributeListToEithers ts) => HMList m ts -> m (HMList Identity ts)
asyncHList Nil = pure Nil
-- asyncHList ts@(_ :. _) = _ <$> asyncList (fromList $ distribute ts)
asyncHList ts@(_ :. _) = fromJust . collect @ts . toList <$> asyncList (fromList $ distribute ts)
-- asyncHList ts@(_ :. _) = collect . fromJust . toList <$> asyncList (fromList $ distribute ts)

data Scheduling m a where
  Pure :: a -> Scheduling m a
  -- Ap :: m (a -> b) -> Scheduling m a -> Scheduling m b
  Ap :: Scheduling m (a -> b) -> m a -> Scheduling m b

instance Functor m => Functor (Scheduling m) where
  fmap f (Pure a) = Pure $ f a
  -- fmap f (Ap mf ma) = Ap (fmap (f .) mf) ma
  fmap f (Ap mf ma) = Ap (fmap (f .) mf) ma

instance Functor m => Applicative (Scheduling m) where
  pure = Pure
  Pure f <*> y = f <$> y
  -- Ap mf ma <*> y = Ap (uncurry <$> mf) $ ( , ) <$> ma <*> y
  Ap mf ma <*> y = Ap (fmap flip mf <*> y) ma

{-
runSchedulingA :: (Applicative m, MonadSchedule m) => Scheduling m a -> m a
runSchedulingA (Pure a) = pure a
runSchedulingA _ = _
-}

{-
type family Fun (ts :: [Type]) a :: Type where
  Fun '[] a = a
  Fun (t ': ts) a = t -> Fun ts a
-}

data Operad (ts :: [Type]) a where
  ONil :: a -> Operad '[] a
  OCons :: (t -> Operad ts a) -> Operad (t ': ts) a

data AppList m a where
  AppList
    :: (DistributeListToEithers ts, CollectEithersToList ts)
    => Operad ts a -> HMList m ts -> AppList m a

{-
schedulingToAppList :: Scheduling m a -> AppList m a
schedulingToAppList (Pure a) = AppList (FunNil a) Nil
schedulingToAppList (Ap smf ma) =
  let AppList f mas = schedulingToAppList smf
  in AppList _ $ ma :. mas
-}

{-
instance Functor (AppList m) where
  fmap f (AppList a Nil) = AppList (f a) Nil
  fmap f (AppList g (ma :. mas)) = _

instance Applicative (AppList m) where
  pure a = AppList a Nil
  _ <*> _ = _
    where
      liftA2 :: (a -> b -> c) -> AppList m a -> AppList m b -> AppList m c
      liftA2 f (AppList a Nil) (AppList b Nil) = AppList (f a b) Nil
      liftA2 f (AppList a Nil) (AppList h (mb :. mbs))
        = let AppList h' mbs' = liftA2 f (AppList a Nil) _
        in AppList _ (mb :. mbs')
      -- liftA2 f (AppList g (a :. as)) (AppList h bs) = AppList _ _
-}

instance Functor (Operad ts) where
  fmap f (ONil a) = ONil $ f a
  fmap f (OCons g) = OCons $ fmap f . g

instance Functor (AppList m) where
  -- fmap f (AppList (FunNil a) Nil) = AppList (FunNil $ f a) Nil
  -- fmap f (AppList (FunCons g) (ma :. mas)) = AppList
  fmap f (AppList fun mas) = AppList (fmap f fun) mas

type family Append ts us where
  Append '[] us = us
  Append (t ': ts) us = t ': Append ts us

funAppend :: (a -> b -> c) -> Operad ts a -> Operad us b -> Operad (Append ts us) c
funAppend f (ONil a) fun = f a <$> fun
funAppend f (OCons g) fun = OCons $ \t -> funAppend f (g t) fun

hmListAppend :: HMList m ts -> HMList m us -> HMList m (Append ts us)
hmListAppend Nil mus = mus
hmListAppend (mt :. mts) mus = mt :. hmListAppend mts mus

instance Applicative (AppList m) where
  pure a = AppList (ONil a) Nil
  (<*>) = liftA2 ($)
    where
      liftA2 :: (a -> b -> c) -> AppList m a -> AppList m b -> AppList m c
      liftA2 f (AppList g mas) (AppList h mbs) = AppList (funAppend f g h) $ hmListAppend mas mbs

identityAppList :: AppList Identity a -> a
identityAppList (AppList (ONil a) Nil) = a
identityAppList (AppList (OCons f) (Identity a :. as)) = identityAppList $ AppList (f a) as

-- Note I only need the Applicative for pure! Add it to MonadSchedule and it becomes a replacement for Applicative!
runAppList :: (Monad m, MonadSchedule m) => AppList m a -> m a
runAppList (AppList (ONil a) Nil) = pure a
runAppList (AppList operad ts) = do
  tsScheduled <- asyncHList ts
  return $ identityAppList $ AppList operad tsScheduled
