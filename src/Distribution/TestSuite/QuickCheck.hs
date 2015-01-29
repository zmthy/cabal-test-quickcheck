{-# LANGUAGE NamedFieldPuns #-}

------------------------------------------------------------------------------
-- | Joins the QuickCheck testing library with Cabal's detailed interface.
module Distribution.TestSuite.QuickCheck
    (
    -- * QuickCheck testing
      testProperty
    , testPropertyWithOptions

    -- * Re-export of Cabal's interface
    , Test
    , testGroup
    ) where

------------------------------------------------------------------------------
import Control.Applicative    ((<$>), (<|>))
import Control.Monad          (foldM)
import Data.List              (isSuffixOf, stripPrefix)
import Data.Maybe             (catMaybes, fromMaybe)
import Distribution.TestSuite hiding (Result)
import Test.QuickCheck


------------------------------------------------------------------------------
-- | Test a QuickCheck property using the current arguments and the given
-- name.
testProperty :: Testable p => String -> p -> Test
testProperty tname prop = Test $ testInstance tname prop stdOptions


------------------------------------------------------------------------------
-- | Test a QuickCheck property using the given arguments and name.
testPropertyWithOptions :: Testable p
                        => String -> Options -> p -> Either String Test
testPropertyWithOptions tname options prop = do
    args <- foldM (uncurry . addToArgs) stdOptions options
    let ti = testInstance tname prop args
    return $ Test ti { run = testRun args prop }


------------------------------------------------------------------------------
-- | A test instance with the given 'Args'.
testInstance :: Testable p => String -> p -> Args -> TestInstance
testInstance tname prop args =
    let ti = TestInstance (testRun args prop)
            tname ["QuickCheck"] qcOptions (addOption args prop ti) in ti


------------------------------------------------------------------------------
-- | Given QuickCheck 'Args' and the property to test, produces a Cabal
-- testing action.
testRun :: Testable p => Args -> p -> IO Progress
testRun args prop = toProgress <$> quickCheckWithResult args prop


------------------------------------------------------------------------------
-- | Adds a Cabal option to the TestInstance.  Fails if QuickCheck does not
-- support the option.
addOption :: Testable p => Args -> p -> TestInstance -> String -> String
          -> Either String TestInstance
addOption args prop test oname value = do
    args' <- addToArgs args oname value
    Right test { run = testRun args' prop }


------------------------------------------------------------------------------
-- | Validates the option and adds it to the given Args value.
addToArgs :: Args -> String -> String -> Either String Args
addToArgs args oname value = do
    option <- lookupOption oname
    otype  <- validOption option value
    case otype of
        Bool -> Right args { chatty = read value }
        Int  -> let int = read value in case oname  of
            "max-success"       -> Right args { maxSuccess = int }
            "max-discard-ratio" -> Right args { maxDiscardRatio = int }
            "max-size"          -> Right args { maxSize = int }
            _                   -> Left $ "Unrecognised option " ++ oname


------------------------------------------------------------------------------
-- | Finds the option in the option list.
lookupOption :: String -> Either String OptionDescr
lookupOption oname = maybe (Left $ "Unrecognised option" ++ oname) Right $
    foldr f Nothing qcOptions
  where f o = (<|> if optionName o == oname then Just o else Nothing)


------------------------------------------------------------------------------
-- | Determines if the option is valid for the given value, returning the
-- type of the option if it is.
validOption :: OptionDescr -> String -> Either String OType
validOption descr value = case optionType descr of
    OptionNumber True (lower, upper) -> do
        testIsInt
        testWithinBounds lower upper
        return Int
    OptionBool                       ->
        if value `elem` ["True", "False"] then return Bool else
        Left $ "Invalid boolean value for option " ++ oname
    _                                ->
        Left $ "Unknown type for option " ++ oname
  where
    oname = optionName descr

    testIsInt = let parse = reads value :: [(Int, String)] in
        if null parse || not (null . snd $ head parse)
            then Left $ "Invalid integer value for option " ++ oname
            else Right ()

    testWithinBounds lower upper = let int = read value in
        if or $ catMaybes [(int >=) <$> lower, (int <=) <$> upper]
            then Left $ "Value out of bounds for option " ++ oname
            else Right ()


------------------------------------------------------------------------------
-- | Converts a QuickCheck 'Result' into a Cabal 'Progress'.
toProgress :: Result -> Progress
toProgress result = Finished $ case result of
    Success {}           -> Pass
    GaveUp {}            -> Fail "Gave up"
    Failure { output }   -> Fail $ tidyFail output
    NoExpectedFailure {} -> Fail "Expected failure when none occurred"

tidyFail :: String -> String
tidyFail output
    | ": \n" `isSuffixOf` suff = take (length suff - 3) suff
    | otherwise                = filter (/= '\n') suff
  where suff = fromMaybe output $ stripPrefix "*** Failed! " output


------------------------------------------------------------------------------
-- | The supported option types.
data OType = Int | Bool


------------------------------------------------------------------------------
-- | The potential QuickCheck options.
qcOptions :: [OptionDescr]
qcOptions =
    [ OptionDescr "max-success" msud (int "1") $ Just "100"
    , OptionDescr "max-discard-ratio" mdrd (int "0") $ Just "10"
    , OptionDescr "max-size" msid (int "1") $ Just "100"
    , OptionDescr "chatty" cd OptionBool $ Just "False"
    ]
  where
    int b = OptionNumber True (Just b, Nothing)
    msud = "Maximum number of successful test before succeeding"
    mdrd = "Maximum number of discarded tests per successful test before \
        \ giving up"
    msid = "Size to use for the biggest test cases"
    cd = "Whether to print anything"


------------------------------------------------------------------------------
-- | The standard QuickCheck 'Args', but with the 'chatty' option turned off.
stdOptions :: Args
stdOptions = stdArgs { chatty = False }
