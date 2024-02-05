{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- base
import Control.Arrow
import Control.Monad
import Data.Functor.Identity
import Data.List.NonEmpty

-- test-framework
import Test.Framework

-- test-framework-hunit
import Test.Framework.Providers.HUnit

-- HUnit
import Test.HUnit hiding (Test)

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2

-- QuickCheck
import Test.QuickCheck

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
