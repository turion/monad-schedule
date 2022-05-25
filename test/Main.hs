{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
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
  , testGroup "MonadSchedule"
    [
    -- , testProperty "Initial order doesn't matter"
    --   $ forAll (scriptsAndShuffled @String) $ \(script, shuffled)
    --     -> snd (essence' (play <$> script)) === snd (essence' (play <$> shuffled))
    -- TODO Except it does, when several schedules all finish at the same time
    -- , testProperty "Initial order doesn't matter" $ forAll ((essence' *** essence') <$> schedulesAndShuffled @String) $ uncurry (===)
    -- TODO Find a cooler property, maybe with Writer
    ]
  ]


-- data Script a = Script a [Integer]
--   deriving Show

-- instance Arbitrary a => Arbitrary (Script a) where
--   arbitrary = Script <$> arbitrary <*> listOf (choose (1, 1000))

-- play :: Script a -> MySchedule a
-- play (Script a waits) = forM_ waits wait >> return a

-- -- TODO Alternative instance for Gen
-- instance Arbitrary a => Arbitrary (MySchedule a) where
--   arbitrary = play <$> arbitrary

-- scriptsAndShuffled :: Arbitrary a => Gen (NonEmpty (Script a), NonEmpty (Script a))
-- scriptsAndShuffled = do
--   scripts <- listOf1 arbitrary
--   shuffled <- shuffle scripts
--   return (fromList scripts, fromList shuffled)

-- schedulesAndShuffled :: Arbitrary a => Gen (NonEmpty (MySchedule a), NonEmpty (MySchedule a))
-- schedulesAndShuffled = do
--   schedules <- listOf1 arbitrary
--   shuffled <- shuffle schedules
--   return (fromList schedules, fromList shuffled)
