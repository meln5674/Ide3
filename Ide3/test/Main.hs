module Main where

import Test.HUnit

import System.Exit

import Tests.ErrorParser
import Tests.SolutionTree

--allTests = TestList [test_parseLogFile "testlog", test_parseLogFile "testlog2"]

allTests = TestList
	[ test_insertProject
	]

main :: IO ()
main = do
    counts <- runTestTT allTests
    if (errors counts /= 0 || failures counts /= 0)
        then exitFailure
        else exitSuccess
