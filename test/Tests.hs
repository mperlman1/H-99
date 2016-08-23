import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Arithmetic 
import System.Exit (ExitCode(..), exitWith)
import Data.Char (toUpper)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList accumulateTests ]

accumulateTests :: [Test]
accumulateTests =
  [ testCase "Problem 31" $
    True @=? isPrime 7
  , testCase "Problem 32" $
    [9, 3, 3] @=? [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
  ]