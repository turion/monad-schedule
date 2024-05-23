{-# LANGUAGE FlexibleInstances #-}

-- test-framework
import Test.Framework

-- monad-schedule (test)
import qualified Trans
import qualified Yield

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ Trans.tests
  , Yield.tests
  ]
