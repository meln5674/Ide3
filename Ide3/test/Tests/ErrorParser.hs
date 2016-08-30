module Tests.ErrorParser where

import Control.Monad

import Data.Maybe

import ErrorParser

import Test.HUnit

test_parseLog :: String -> IO String -> Test
test_parseLog logName getLogContents = TestLabel logName $ TestCase $ do
    logContents <- getLogContents
    assertBool "Parsing failed" $ isJust $ parseLog logContents

test_parseLogFile :: FilePath -> Test
test_parseLogFile logPath = test_parseLog logPath $ readFile logPath
