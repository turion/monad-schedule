{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OSThreadPool where

-- base
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.List.NonEmpty as NonEmpty

-- test-framework
import Test.Framework

-- test-framework-QuickCheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- QuickCheck
import Test.QuickCheck (forAll, shuffle, (===), ioProperty, withMaxSuccess)

-- monad-schedule
import Control.Monad.Schedule.Class (scheduleAndFinish)
import Control.Monad.Schedule.OSThreadPool

myWait :: Int -> OSThreadPool 20 Int
myWait n = do
  liftIO $ threadDelay $ n * 1000
  return n

orderedTimes :: [Int]
orderedTimes = [0, 100..1000]

tests =
  testGroup
    "OSThreadPool"
    [ testProperty "Actions are scheduled chronologically" $ withMaxSuccess 5 $ forAll (shuffle orderedTimes) $ \times -> ioProperty $ do
        results <- unOSThreadPool $ scheduleAndFinish $ myWait <$> NonEmpty.fromList times
        return $ results === NonEmpty.fromList orderedTimes
    ]
