{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Monad.Schedule.OSThreadPool
  (OSThreadPool (..))
  where

-- base
import Control.Concurrent
import Control.Monad (forM, replicateM)
import Control.Monad.IO.Class
import Data.Either (partitionEithers)
import Data.List.NonEmpty hiding (cycle, zip)
import Data.Proxy
import GHC.TypeLits
import Prelude hiding (take)

-- stm
import Control.Concurrent.STM

-- monad-schedule
import Control.Monad.Schedule.Class
import Data.Functor.Compose (Compose (..))

newtype OSThreadPool (n :: Nat) a = OSThreadPool {unOSThreadPool :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

proxyForAction :: OSThreadPool n a -> Proxy n
proxyForAction _ = Proxy

data WorkerLink a = WorkerLink
  { jobTChan :: TChan (IO a)
  , resultTChan :: TChan a
  , threadId :: ThreadId
  }

putJob :: WorkerLink a -> OSThreadPool n a -> IO ()
putJob WorkerLink {..} OSThreadPool {..} =
  atomically $
    writeTChan jobTChan $
      unOSThreadPool

makeWorkerLink :: OSThreadPool n (WorkerLink a)
makeWorkerLink = OSThreadPool $ do
  jobTChan <- atomically newTChan
  resultTChan <- atomically newTChan
  let worker = do
        job <- atomically $ readTChan jobTChan
        result <- job
        atomically $ writeTChan resultTChan result
        worker
  threadId <- forkOS worker
  return WorkerLink {..}

type WorkerLinks = Compose [] WorkerLink

instance (KnownNat n, (1 <=? n) ~ True) => MonadSchedule (OSThreadPool n) where
  type SchedulingContext (OSThreadPool n) = WorkerLinks -- FIXME Vec n

  createContext :: forall n a . KnownNat n => OSThreadPool n (WorkerLinks a)
  createContext = do
    let n = natVal (Proxy @n)
    Compose <$> replicateM (fromInteger n) makeWorkerLink

  scheduleWithContext (Compose workerLinks) actions = OSThreadPool $ do
    backgroundActions <- forM (zip (cycle workerLinks) (toList actions)) $
      \(link, action) -> do
        putJob link action
        return $ resultTChan link
    pollPools backgroundActions
    where
      pollPools :: [TChan a] -> IO (NonEmpty a, [OSThreadPool n a])
      pollPools chans = do
        results <- traverse pollPool chans
        case partitionEithers results of
          (_, []) -> do
            threadDelay 1000 -- FIXME tune down? Or just yield?
            pollPools chans
          (remainingChans, a : as) ->
            return
              ( a :| as
              , OSThreadPool . atomically . readTChan <$> remainingChans
              )

      pollPool :: TChan a -> IO (Either (TChan a) a)
      pollPool chan = maybe (Left chan) Right <$> atomically (tryReadTChan chan)
