{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Class where

-- base
import Control.Concurrent
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
  scheduleAndFinish,
 )
import Data.List (sort)

myWait :: Int -> IO Int
myWait n = do
  threadDelay $ n * 1000
  return n

orderedTimes :: [Int]
orderedTimes = [0, 100 .. 1000]

tests :: Test
tests =
  testGroup
    "IO"
    [ testProperty "Actions are scheduled chronologically" $ withMaxSuccess 5 $ forAll (shuffle orderedTimes) $ \times -> ioProperty $ do
        results <- scheduleAndFinish $ myWait <$> NonEmpty.fromList times
        return $ results === NonEmpty.fromList orderedTimes
    , -- This doesn't work: https://github.com/turion/rhine/issues/365
      -- , testCase "Scheduling with continuations works" $ do
      --     results <- scheduleProgramsWithoutContext [arithmeticSequence 300 5, arithmeticSequence 500 3]
      --     assertEqual "ordered" (sort results) (reverse results)
      testCase "Scheduling with continuations works when using scheduleProgramsWithContext" $ do
        results <- scheduleProgramsWithContext [arithmeticSequence 300 5, arithmeticSequence 500 3]
        assertEqual "ordered" (sort results) (reverse results)
    ]

newtype Program = Program {getProgram :: IO (Maybe Int, Maybe Program)}

scheduleProgramsWithoutContext :: NonEmpty Program -> IO [Maybe Int]
scheduleProgramsWithoutContext programs = do
  (stepped, continuations) <- schedule $ getProgram <$> programs
  let (msgs, maybePrograms) = unzip $ toList stepped
  case catMaybes maybePrograms ++ (Program <$> continuations) of
    [] -> return msgs
    (p : ps) -> fmap (msgs ++) $ scheduleProgramsWithoutContext $ p :| ps

scheduleProgramsWithContext :: NonEmpty Program -> IO [Maybe Int]
scheduleProgramsWithContext programs = do
  context <- createContext
  (stepped, continuations) <- scheduleWithContext context $ getProgram <$> programs
  let (msgs, maybePrograms) = unzip $ toList stepped
  case catMaybes maybePrograms ++ (Program <$> continuations) of
    [] -> return msgs
    (p : ps) -> fmap (msgs ++) $ scheduleProgramsWithContext $ p :| ps

arithmeticSequence ::
  -- | Step size
  Int ->
  -- | Number of steps
  Int ->
  Program
arithmeticSequence _ nSteps | nSteps <= 0 = Program $ return (Nothing, Nothing)
arithmeticSequence stepSize nSteps = Program $ do
  threadDelay $ 1000 * stepSize
  return (Just nSteps, Just $ arithmeticSequence stepSize $ nSteps - 1)

-- scheduleWithContextIO var as conts = do
--   forM_ as $ \action -> forkIO $ putMVar var =<< action
--   a <- takeMVar var
--   as' <- drain var
--   let remaining = replicate (length as - 1 - length as') $ takeMVar var
--   return (a :| as', remaining)
--   where
--     drain :: MVar a -> IO [a]
--     drain var = do
--       aMaybe <- tryTakeMVar var
--       case aMaybe of
--         Just a -> do
--           as' <- drain var
--           return $ a : as'
--         Nothing -> return []
