module Util where

-- base
import Data.List.NonEmpty

-- QuickCheck
import Test.QuickCheck

-- FIXME Why is this not in QuickCheck?
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
