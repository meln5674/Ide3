{-# LANGUAGE ImplicitParams #-}
module Tests.Import.Basic (tests_basicImport) where

import GHC.Stack

import Test.HUnit

import Tests.Utils

tests_basicImport = TestList
    [ test_addImport
    , test_addAndRemoveImport
    , test_addUnionImport
    , test_addDuplicateImport
    ]

test_addImport :: (?loc :: CallStack) => Test    
test_addImport = unimplementedTest

test_addAndRemoveImport :: (?loc :: CallStack) => Test    
test_addAndRemoveImport = unimplementedTest

test_addUnionImport :: (?loc :: CallStack) => Test    
test_addUnionImport = unimplementedTest

test_addDuplicateImport :: (?loc :: CallStack) => Test    
test_addDuplicateImport = unimplementedTest
