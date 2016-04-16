{-# LANGUAGE ImplicitParams #-}
module Tests.Import.NonExistent (tests_nonExistentImport) where

import GHC.Stack

import Test.HUnit

import Tests.Utils

tests_nonExistentImport = TestList
    [ test_addImportToNonExistentModule
    , test_removeNonExistentImport
    ]

test_addImportToNonExistentModule :: (?loc :: CallStack) => Test
test_addImportToNonExistentModule = unimplementedTest

test_removeNonExistentImport :: (?loc :: CallStack) => Test    
test_removeNonExistentImport = unimplementedTest
