{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Class where

-- base
import Control.Concurrent
import Control.Monad.IO.Class
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty hiding (unzip)
import Data.Maybe (catMaybes)

-- test-framework
import Test.Framework

-- test-framework-HUnit
import Test.Framework.Providers.HUnit (testCase)

-- HUnit
import Test.HUnit (assertEqual)

-- test-framework-QuickCheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- QuickCheck
import Test.QuickCheck (forAll, ioProperty, shuffle, withMaxSuccess, (===))

-- monad-schedule
import Control.Monad.Schedule.Class (
  MonadSchedule (..),
  runFunnyIO,
  scheduleAndFinish,
 )

-- monad-schedule (test)
import Util

myWait :: (MonadIO m) => Int -> m Int
myWait n = do
  liftIO $ threadDelay $ n * 1000
  return n

orderedTimes :: [Int]
orderedTimes = [0, 100 .. 1000]

programs :: MonadIO m => NonEmpty (Program m)
programs = [arithmeticSequence 30 100, arithmeticSequence 50 60]

tests :: Test
tests =
  testGroup
    "Class"
    [ testGroup
        "IO"
        [ testProperty "Actions are scheduled chronologically" $ withMaxSuccess 3 $ forAll (shuffle orderedTimes) $ \times -> ioProperty $ do
            results <- scheduleAndFinish $ myWait <$> NonEmpty.fromList times
            return $ results === NonEmpty.fromList orderedTimes
        , testPrograms id programs
        , test2Programs id programs
        ]
    , testGroup
        "FunnyIO"
        [ testProperty "Actions are scheduled chronologically" $ withMaxSuccess 3 $ forAll (shuffle orderedTimes) $ \times -> ioProperty $ do
            results <- runFunnyIO $ scheduleAndFinish $ myWait <$> NonEmpty.fromList times
            return $ results === NonEmpty.fromList orderedTimes
        , testPrograms runFunnyIO programs
        , test2Programs runFunnyIO programs
        ]
    ]
