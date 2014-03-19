module CabalQuickCheckExample where

import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck

tests :: IO ([Test])
tests = return [
        testProperty "FailingProperty" prop_fail,
        testProperty "SucceedingProperty" prop_succeed,
        testProperty "SometimesPassingProperty" prop_positiveNotEqualsNegative
    ]
    where
        prop_succeed :: Int -> Bool
        prop_succeed _ = True

        prop_fail :: Int -> Bool
        prop_fail _ = False

        prop_positiveNotEqualsNegative :: Int -> Bool
        prop_positiveNotEqualsNegative x = x /= -x
