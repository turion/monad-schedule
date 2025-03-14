{-# LANGUAGE FlexibleInstances #-}

-- test-framework
import Test.Framework

-- monad-schedule (test)
import qualified Class
import qualified FreeAsync
import qualified OSThreadPool
import qualified Trans
import qualified Yield

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Class.tests
  , FreeAsync.tests
  , OSThreadPool.tests
  , Trans.tests
  , Yield.tests
  ]
