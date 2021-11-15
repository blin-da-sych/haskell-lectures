{-# OPTIONS -Wall #-}

import            Lectures.ScndLecture  (solveSquare)
import            Test.Tasty            (TestTree, defaultMain, testGroup)
import            Test.Tasty.HUnit      (assertBool, testCase)

main :: IO ()
main = defaultMain tests

-- | Test suite for lectures. Run: @stack test --ta=-j4@
tests :: TestTree
tests =
  testGroup
    "Lecture tests:"
    [ testCase "- Square Equation: x² + 2x + 1 = 0 -> [-1.0, -1.0]" $
        assertBool "[-1.0, -1.0] are expected" $ 
          solveSquare 1 2 1 == [-1.0, -1.0]
    , testCase "- Square Equation: x² - 2x + 1 = 0 -> [ 1.0,  1.0]" $
        assertBool "[1.0, 1.0] are expected" $ 
          solveSquare 1 (-2) 1 == [1.0, 1.0]
    ]

-- $> main
