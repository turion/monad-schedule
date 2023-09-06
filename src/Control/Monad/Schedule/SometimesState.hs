{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Schedule.SometimesState where
import Control.Monad.Operational
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Schedule.Class
import Control.Category ((>>>))
import Control.Arrow (first, second)
import Data.List.NonEmpty (NonEmpty (..), toList, fromList)
import Data.Either (partitionEithers)
import Control.Monad (join)
import Data.Typeable
import Control.Monad.State.Class
import Control.Monad.State.Lazy -- FIXME strict?
import Data.Maybe (fromJust)

data SometimesStateE s a where
  Modify :: State s a -> SometimesStateE s a

-- TODO this blocks for _all_ var modifications, even on independent vars
newtype SometimesStateT s m a = SometimesStateT
  { getSometimesStateT :: ProgramT (SometimesStateE s) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

runSometimesStateT :: Monad m => SometimesStateT s m a -> s -> m (a, s)
runSometimesStateT SometimesStateT {getSometimesStateT} s = do
  tip <- viewT getSometimesStateT
  case tip of
    Return a -> return (a, s)
    Modify f :>>= g -> let (b, s') = runState f s in runSometimesStateT (SometimesStateT $ g b) s'

instance (MonadSchedule m, Monad m) => MonadSchedule (SometimesStateT s m) where
  schedule
    = fmap getSometimesStateT
    >>> schedule
    >>> fmap (second (fmap SometimesStateT))
    >>> SometimesStateT

instance Monad m => MonadState s (SometimesStateT s m) where
  state = SometimesStateT . singleton . Modify . state

data VarContent = forall a . Typeable a => VarContent { getVarContent :: Maybe a }

newtype Var a = Var { getVar :: Int }

newtype Store = Store { getStore :: [VarContent] }

emptyStore :: Store
emptyStore = Store mempty

newVar :: forall m a . (Monad m, Typeable a) => SometimesStateT Store m (Var a)
newVar = state $ \Store { getStore } ->
  let index = length getStore
      var = Var index
      content = VarContent (Nothing @(Maybe a))
  in (var, Store $ getStore ++ [content])
     -- FIXME Yes, use sequence

onVar:: (Typeable a, Monad m) => State (Maybe a) b -> Var a -> SometimesStateT Store m b
onVar action Var { getVar } = state $ \Store { getStore} ->
  let (before, content : after) = splitAt getVar getStore
      a = case content of
        VarContent (Just a) -> Just $ fromJust $ cast a
        VarContent Nothing -> Nothing
      (b, content') = runState action a
      store' = before ++ VarContent content' : after
  in (b, Store store')

tryTakeVar :: forall m a . (Monad m, Typeable a) => Var a -> SometimesStateT Store m (Maybe a)
tryTakeVar = onVar $ state $ \case
  Nothing -> (Nothing, Nothing)
  Just a -> (Just a, Nothing)

takeVar :: (Monad m, Typeable a) => Var a -> SometimesStateT Store m a
takeVar var = retry $ tryTakeVar var

tryPutVar :: (Monad m, Typeable a) => Var a -> a -> SometimesStateT Store m (Maybe ())
tryPutVar var a = flip onVar var $ state $ \case
  Nothing -> (Just (), Just a)
  Just a -> (Nothing, Just a)

putVar :: (Monad m, Typeable a) => Var a -> a -> SometimesStateT Store m ()
putVar var a = retry $ tryPutVar var a

retry :: Monad m => SometimesStateT Store m (Maybe a) -> SometimesStateT Store m a
retry action = action >>= maybe (retry action) pure

splitStuff :: Monad m => [ProgramViewT e m a] -> ([a], [ProgramT e m a])
splitStuff = partitionEithers . fmap returnOrYield

returnOrYield :: Monad m => ProgramViewT e m a -> Either a (ProgramT e m a)
returnOrYield (Return a) = Left a
returnOrYield (instruction :>>= f) = Right $ singleton instruction >>= f

instance (Monad m, MonadSchedule m) => MonadSchedule (ProgramT e m) where
  schedule = scheduleInteract

scheduleInteract :: (MonadSchedule m, Monad m) => NonEmpty (ProgramT e m a) -> ProgramT e m (NonEmpty a, [ProgramT e m a])
scheduleInteract actions = do
  (mFinished, mRunning) <- lift $ schedule $ viewT <$> actions
  let (eFinished, eRunning) = splitStuff2 $ toList mFinished
      mRunningT = join . lift . fmap unviewT <$> mRunning
  case eFinished of
    [] -> do
      steps <- runSteps eRunning
      scheduleInteract $ fromList $ mRunningT ++ steps
    (a : as) -> return (a :| as, (runStep <$> eRunning) ++ mRunningT)

data Step e m a = forall b . Step
  { instruction :: e b
  , continuation :: b -> ProgramT e m a
  }

runStep :: Monad m => Step e m a -> ProgramT e m a
runStep Step { instruction, continuation } = singleton instruction >>= continuation

splitStuff2 :: Monad m => [ProgramViewT e m a] -> ([a], [Step e m a])
splitStuff2 = partitionEithers . fmap returnOrStep
  where
    returnOrStep (Return a) = Left a
    returnOrStep (instruction :>>= continuation) = Right Step { instruction, continuation }

-- | Width-first traversal
runSteps :: Monad m => [Step e m a] -> ProgramT e m [ProgramT e m a]
runSteps [] = return []
runSteps (Step { instruction, continuation} : steps) = do
  doStep <- singleton instruction
  doneSteps <- runSteps steps
  return (continuation doStep : doneSteps)
