{-# LANGUAGE ScopedTypeVariables #-}
module Trans.IO where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.List (sort)
import Data.List.NonEmpty hiding (sort, scanl1)

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)

-- time
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

-- QuickCheck
import Test.QuickCheck ((===), Testable (property), Positive (..), counterexample, withMaxSuccess)
import Test.QuickCheck.Monadic (monadicIO, assert, monitor)

-- test-framework
import Test.Framework

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- monad-schedule
import Control.Monad.Schedule.Trans (ScheduleT, runScheduleT)
import qualified Control.Monad.Schedule.Trans as Schedule
import Util ()
import Control.Monad.Schedule.Class (scheduleAndFinish)

-- Integer represent steps of 10 ms
type MySchedule a = ScheduleT Integer (WriterT [UTCTime] IO) a

wait :: Integer -> MySchedule ()
wait n = do
  Schedule.wait n
  liftIO (threadDelay $ fromInteger n * 100000)
  time <- liftIO getCurrentTime
  lift $ tell [time]

runMySchedule :: MySchedule a -> IO (a, [Integer])
runMySchedule prog = do
  initialTime <- getCurrentTime
  (a, times) <- runWriterT $ runScheduleT (const $ return ()) prog
  return (a, round . (* 10) . flip diffUTCTime initialTime <$> times)

tests = testGroup "IO"
  [ testProperty "Waits correctly even if threads block" $ withMaxSuccess 10 $ \(waits :: NonEmpty [Positive Integer]) -> monadicIO $ do
      let waits' = fmap getPositive <$> waits
      (_, observed) <- liftIO $ runMySchedule $ scheduleAndFinish $ mapM wait <$> waits'
      let expected = sort $ concatMap (scanl1 (+)) waits'
      monitor $ counterexample $ unlines
        [ "threads: " ++ show (toList waits')
        , "expected: " ++ show expected
        , "observed: " ++ show observed
        ]
      assert $ observed == expected
  ]
