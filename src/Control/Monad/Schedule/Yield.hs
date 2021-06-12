
-- * 'YieldT'

-- | A monad for scheduling with cooperative concurrency.
type YieldT = ScheduleT ()

-- | Let another thread wake up.
yield :: Monad m => YieldT m ()
yield = wait ()

-- | Run a 'YieldT' value in a 'MonadIO',
--   interpreting 'yield's as GHC concurrency yields.
runYieldIO
  :: MonadIO m
  => YieldT m a -> m a
runYieldIO = runScheduleT $ const $ liftIO $ C.yield
