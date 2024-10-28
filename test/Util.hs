{-# LANGUAGE RankNTypes #-}
module Util where

-- base

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)

-- monad-schedule
import Control.Monad.Schedule.Class
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, (@?=))
import Data.List (sort)

newtype Program m = Program {getProgram :: m (Int, Maybe (Program m))}

scheduleProgramsWithoutContext :: (Monad m, MonadSchedule m) => NonEmpty (Program m) -> m [Int]
scheduleProgramsWithoutContext programs = do
  (stepped, continuations) <- schedule $ getProgram <$> programs
  let (msgs, maybePrograms) = unzip $ toList stepped
  case catMaybes maybePrograms ++ (Program <$> continuations) of
    [] -> return msgs
    (p : ps) -> fmap (msgs ++) $ scheduleProgramsWithoutContext $ p :| ps

scheduleProgramsWithContext :: (Monad m, MonadSchedule m) => NonEmpty (Program m) -> m [Int]
scheduleProgramsWithContext programs = do
  context <- createContext
  go context programs
  where
    go context programs = do
      (stepped, continuations) <- scheduleWithContext context $ getProgram <$> programs
      let (msgs, maybePrograms) = unzip $ toList stepped
      case catMaybes maybePrograms ++ (Program <$> continuations) of
        [] -> return msgs
        (p : ps) -> fmap (msgs ++) $ go context $ p :| ps

finish :: Monad m => m (Int, Maybe (Program m)) -> m [Int]
finish running = do
  (n, prog) <- running
  maybe (return [n]) (fmap (n :) . finish . getProgram ) prog

schedule2ProgramsWithContext :: (Monad m, MonadSchedule m) => Program m -> Program m -> m [Int]
schedule2ProgramsWithContext p1 p2 = do
  context <- createContext
  go context p1 p2
  where
    go context p1 p2 = do
      result <- race (getProgram p1) (getProgram p2)
      case result of
        Left ((n, Nothing), running) -> (n :) <$> finish running
        Left ((n, Just p1'), running) -> (n :) <$> go context p1' (Program running)
        Right (running, (n, Nothing)) -> (n :) <$> finish running
        Right (running, (n, Just p2')) -> (n :) <$> go context (Program running) p2'

arithmeticSequenceM ::
  -- | Step size
  Monad m => Int ->
  -- | Number of steps
  Int ->
  -- | Waiting instruction
  (Integer -> m ()) ->
  Program m
arithmeticSequenceM stepSize nSteps myWait = go 0
  where
    go n | n >= nSteps = Program $ return (n * stepSize, Nothing)
    go n = Program $ do
      myWait $ toInteger stepSize
      return ((n + 1) * stepSize, Just $ go $ n + 1)

arithmeticSequence ::
  (MonadIO m) =>
  -- | Step size
  Int ->
  -- | Number of steps
  Int ->
  Program m
arithmeticSequence stepSize nSteps = arithmeticSequenceM stepSize nSteps $ liftIO . threadDelay . fromInteger . (1000 *)

testPrograms :: (Monad m, MonadSchedule m) => (forall x . m x -> IO x) -> NonEmpty (Program m) -> Test
testPrograms run programs =
  testGroup
    "testPrograms"
    [ testCase "Scheduling with continuations works" $ do
        results <- run $ scheduleProgramsWithoutContext programs
        sort results @?= results
    , testCase "Scheduling with continuations works when using scheduleProgramsWithContext" $ do
        results <- run $ scheduleProgramsWithContext programs
        sort results @?= results
    ]
