{-# LANGUAGE RankNTypes #-}

-- base

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (catMaybes)

-- monad-schedule
import Control.Monad.Schedule.Class
import Control.Monad.Schedule.Trans

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
