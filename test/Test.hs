{-# OPTIONS -Wall #-}

import           Lectures.ScndLecture  (solveSquare)
import           Practice.SxthPractice (Expression (Abstraction, Application, Variable))

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase)

main :: IO ()
main = defaultMain tests

-- | Test suite for lectures. Run: @stack test --ta=-j4@
tests :: TestTree
tests =
  testGroup
    "Lecture tests:"
    [ testCase "- Square Equation:      x² + 2x + 1 = 0 -> [-1.0, -1.0]" $
        assertBool "[-1.0, -1.0] are expected" $
          solveSquare 1 2 1 == [-1.0, -1.0]
    , testCase "- Square Equation:      x² - 2x + 1 = 0 -> [ 1.0,  1.0]" $
        assertBool "[1.0, 1.0] are expected" $
          solveSquare 1 (-2) 1 == [1.0, 1.0]
    , testCase "- λ-Calculus imitation: \"\\y -> \\x -> x y\"" $
        assertBool "\"\\y -> \\x -> x y\" are expected" $
          show (Abstraction "y" (Abstraction "x" (Application (Variable "x") (Variable "y"))))
          == "\\y -> \\x -> x y"
    ]

-- $> main
