{-# LANGUAGE ImplicitParams #-}
module Tests.Import.NonExistent (tests_nonExistentImport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_nonExistentImport = TestList
    [ test_addImportToNonExistentModule
    , test_removeNonExistentImport
    ]

test_addImportToNonExistentModule :: (?loc :: CallStack) => Test
test_addImportToNonExistentModule = expectFailure $ do
    addImport nonExistentModuleInfo newImport

test_removeNonExistentImport :: (?loc :: CallStack) => Test    
test_removeNonExistentImport = expectFailure $ do
    createModule newModuleInfo
    removeImport newModuleInfo nonExistentImportId
