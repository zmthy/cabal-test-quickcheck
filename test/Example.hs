------------------------------------------------------------------------------
module Example (tests) where

------------------------------------------------------------------------------
import Distribution.TestSuite.QuickCheck


------------------------------------------------------------------------------
tests :: IO [Test]
tests = return
    [ testProperty "Succeeds" True
    , testProperty "Fails" False
    , testGroup "May fail" mayFail
    ]


------------------------------------------------------------------------------
mayFail :: [Test]
mayFail =
    [ testProperty "Maybe fails" neqNegation
    , testProperty "Probably fails" $ not . neqNegation
    ]

neqNegation :: Int -> Bool
neqNegation x = x /= -x

