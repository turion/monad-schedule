{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trans where

-- base
-- base
import Control.Monad (forever, void)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer (Writer, tell, runWriter, execWriter)

-- QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck

-- test-framework
import Test.Framework

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- HUnit
import Test.HUnit hiding (Test)

-- monad-schedule
import Control.Monad.Schedule.Trans
import Control.Monad.Schedule.Class (scheduleAndFinish)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Control.Arrow
import Control.Monad.Free (_Free)

sampleActions :: NonEmpty (MySchedule ())
sampleActions = [wait 23, wait 42]

tests = testGroup "Trans"
  [ testCase "Only leftover time is waited"
    $ assertRunsLike sampleActions [Waited 23, Waited (42 - 23)]
  , testCase "Scheduling two waits"
    $ assertRunsEqual sampleActions (NonEmpty.reverse sampleActions)
  , testCase "Different number of waits"
    $ assertRunsLike
      [ myLog "Thread 1 starts" >> wait 5 >> myLog "Thread 1 action" >> wait 5 >> myLog "Thread 1 done"
      , myLog "Thread 2 starts" >> wait 7 >> myLog "Thread 2 done"
      ]
      [ Log "Thread 1 starts"
      , Log "Thread 2 starts"
      , Waited 5
      , Log "Thread 1 action"
      , Waited 2
      , Log "Thread 2 done"
      , Waited 3
      , Log "Thread 1 done"
      ]
  , testCase "Blocking thread doesn't starve other thread (positive wait times)"
    $ assertRunContains
      [ forever $ myLog "Busy loop starts" >> wait 1 >> myLog "Busy loop ends"
      , myLog "One off thread starts" >> wait 2 >> myLog "One off thread does a thing" >> wait 1 >> myLog "One off thread done"
      ]
      $ Log "One off thread done"
  , testCase "Blocking thread doesn't starve other thread (0 waits)"
    $ assertRunContains
      [ forever $ myLog "Busy loop starts" >> wait 0 >> myLog "Busy loop ends"
      , myLog "One off thread starts" >> wait 0 >> myLog "One off thread does a thing" >> wait 0 >> myLog "One off thread done"
      ]
      $ Log "One off thread done"
  , testProperty "Every thread is eventually woken up"
    $ withMaxSuccess 1000
    $ \(scripts :: Scripts) (skip :: Positive Int) ->
    let steps
          -- In principle, every iteration of the whole script, every thread should be woken up, but allow for some extra overhead
          = take (3 * sizeScripts scripts + 3)
          -- Randomly skip some steps ahead
          $ drop (getPositive skip)
          $ runMySchedule $ interpretScripts scripts
    in counterexample ("steps: " ++ show steps)
    $ conjoin $ map (Log >>> (`elem` steps)) $ NonEmpty.toList $ threadNames scripts
  ]

assertRunsEqual :: NonEmpty (MySchedule a1) -> NonEmpty (MySchedule a2) -> Assertion
assertRunsEqual actions1 actions2 = assertEqual "Should run the same under scheduling" (runMySchedule actions1) (runMySchedule actions2)

assertRunsLike :: NonEmpty (MySchedule a) -> [Event] -> Assertion
assertRunsLike actions events = assertEqual "Should run like the following under scheduling" events $ runMySchedule actions

assertRunContains :: NonEmpty (MySchedule a) -> Event -> Assertion
assertRunContains actions event = assertBool ("The run should contain the event " ++ show event) $ event `elem` runMySchedule actions

assertInitiallyRunsLike :: NonEmpty (MySchedule a) -> [Event] -> Assertion
assertInitiallyRunsLike actions events = assertEqual "Should, at the beginning, run like the following under scheduling" events $ take (length events) $ runMySchedule actions

data Event
  = Log String
  | Waited Integer
  deriving (Eq, Show)

type MySchedule a = ScheduleT Integer (Writer [Event]) a

myLog :: String -> MySchedule ()
myLog = lift . tell . pure . Log

runMySchedule :: NonEmpty (MySchedule a) -> [Event]
runMySchedule = execWriter . runScheduleT (tell . pure . Waited) . scheduleAndFinish

data Script = Script
  { prefix :: [Positive Integer]
  , loop :: NonEmpty (Positive Integer)
  , threadName :: String
  }
  deriving Show

-- FIXME Why is this not in QuickCheck?
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (NonEmpty.:|) <$> arbitrary <*> arbitrary


genScript :: ThreadName -> Gen Script
genScript threadName = do
  prefix <- arbitrary
  loop <- arbitrary
  return Script { .. }

instance Arbitrary Scripts where
  arbitrary = do
    nScripts <- getPositive <$> (arbitrary :: Gen (Positive Integer))
    getScripts <- mapM genScript $ show <$> NonEmpty.fromList [1..nScripts]
    return Scripts { .. }

newtype Scripts = Scripts { getScripts :: NonEmpty Script }
  deriving Show

type ThreadName = String

interpretScript :: Script -> MySchedule ()
interpretScript Script { .. } = do
  let perform interval = myLog threadName >> wait (getPositive interval)
  mapM_ perform prefix
  forever $ mapM_ perform loop

interpretScripts :: Scripts -> NonEmpty (MySchedule ())
interpretScripts = NonEmpty.map interpretScript . getScripts

sizeScript :: Script -> Int
sizeScript Script { .. } = fromInteger $ sum (getPositive <$> prefix) + sum (getPositive <$> loop)

sizeScripts :: Scripts -> Int
sizeScripts = sum . fmap sizeScript . getScripts

threadNames :: Scripts -> NonEmpty ThreadName
threadNames = fmap threadName . getScripts
