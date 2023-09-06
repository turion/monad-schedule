{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}
module SometimesState where

-- base
import Data.Functor.Identity

-- test-framework
import Test.Framework ( testGroup )

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- monad-schedule
import Control.Monad.Schedule.SometimesState
import Data.List.NonEmpty (NonEmpty)
import Control.Monad.Schedule.Class
import Test.HUnit (assertEqual, (~=?), (@?=), (@=?))
import Control.Monad.Operational
import Control.Monad.State.Lazy
import Debug.Trace (traceShow)
import Control.Monad (replicateM)
import Data.Functor (void)

type MySchedule a = SometimesStateT Store Identity a

runMySchedule :: MySchedule (NonEmpty (MySchedule a)) -> NonEmpty a
runMySchedule program = fst . runIdentity . ($ emptyStore) . runSometimesStateT $ do
  threads <- program
  scheduleAndFinish threads

runSometimesStateTUntil :: Monad m => Int -> SometimesStateT s m a -> s -> m (Maybe a, s)
runSometimesStateTUntil n action s | n <= 0 = return (Nothing, s)
runSometimesStateTUntil n SometimesStateT {getSometimesStateT} s = do
  tip <- viewT getSometimesStateT
  case tip of
    Return a -> return (Just a, s)
    Modify f :>>= g -> let (b, s') = runState f s in runSometimesStateTUntil (n - 1) (SometimesStateT $ g b) s'

runMyScheduleUntil :: Int -> MySchedule (NonEmpty (MySchedule a)) -> Maybe (NonEmpty a)
runMyScheduleUntil n program = fst . runIdentity . ($ emptyStore) . runSometimesStateTUntil n $ do
  threads <- program
  scheduleAndFinish threads


tests = testGroup "SometimesState"
  [ testCase "Can put and take a var" $ do
    (Just [0 :: Int, 23 :: Int] @=?) $ runMyScheduleUntil 100 $ do
      var <- newVar
      return
        [ do
            putVar var 23
            return 0
        , takeVar var
        ]
  , testCase "Can put and take a var several times" $ do
    (Just [200 :: Int, 23, 100, 42] @=?) $ runMyScheduleUntil 100 $ do
      var <- newVar
      return
        [ takeVar var
        , takeVar var
        , putVar var 23 >> return 200
        , putVar var 42 >> return 100
        ]
  , testCase "Can put and take a var several times (different order)" $ do
    (Just [100 :: Int, 42, 200, 23] @=?) $ runMyScheduleUntil 100 $ do
      var <- newVar
      return
        [ takeVar var
        , putVar var 42 >> return 100
        , takeVar var
        , putVar var 23 >> return 200
        ]
  , testCase "Timeout on deadlock (takeVar)" $ do
    (Nothing @=?) $ runMyScheduleUntil 100 $ do
      var <- newVar :: MySchedule (Var Int)
      return
        [ takeVar var
        ]
  , testCase "Timeout on deadlock (putVar)" $ do
    (Nothing @=?) $ runMyScheduleUntil 100 $ do
      var <- newVar
      return
        [ putVar var (42 :: Int)
        , putVar var 23
        ]
  , testCase "Can hop over several vars" $ do
    (Just ["1", "2", "3", "4", "5", "Hello world, here I am"] @=?) $ runMyScheduleUntil 100 $ do
      var1 <- newVar
      var2 <- newVar
      var3 <- newVar
      var4 <- newVar
      var5 <- newVar
      return
        [ takeVar var5
        , do
            putVar var1 "Hello"
            return "1"
        , do
            hello <- takeVar var1
            putVar var2 $ hello ++ " world,"
            return "2"
        , do
            hello <- takeVar var2
            putVar var3 $ hello ++ " here"
            return "3"
        , do
            hello <- takeVar var3
            putVar var4 $ hello ++ " I"
            return "4"
        , do
            hello <- takeVar var4
            putVar var5 $ hello ++ " am"
            return "5"
        ]
  , testCase "Stress testing 1000 puts" $ do
    (Just [1000, 1000] @=?) $ runMyScheduleUntil 10000 $ do
      var <- newVar
      return
        [ sum <$> replicateM 1000 (takeVar var)
        , length <$> replicateM 1000 (putVar var 1)
        ]
  ]
