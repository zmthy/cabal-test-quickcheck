------------------------------------------------------------------------------
-- | An example testing module for cabal-test-quickcheck. The `test-module` of
-- a test-suite must export a 'tests' definition.
module Example (tests) where

import           Distribution.TestSuite.QuickCheck


------------------------------------------------------------------------------
-- | The root definition of the tests. Must be of type 'IO [Test]'.
tests :: IO [Test]
tests = return
    [ testProperty "Succeeds" True
    , testProperty "Fails" False
    , mayFail
    ]


------------------------------------------------------------------------------
-- | A list of tests can be grouped together into a single test.
mayFail :: Test
mayFail = testGroup "May fail"
    [ testProperty "Maybe fails" neqNegation
    , testProperty "Probably fails" (not . neqNegation)
    ]

neqNegation :: Int -> Bool
neqNegation x = x /= -x
