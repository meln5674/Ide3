{-# LANGUAGE ImplicitParams #-}
module Tests.Export.NonExistent (tests_nonExistentExport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_nonExistentExport = TestList
    [ test_addExportToNonExistentModule
    , test_removeExportFromNonExistentModule
    , test_exportAllNonExistentModule
    , test_removeNonExistentExport
    , test_addExportWithoutDeclaration
    ]

test_addExportToNonExistentModule :: (?loc :: CallStack) => Test    
test_addExportToNonExistentModule = expectFailure $ do
    addExport nonExistentModuleInfo newExport

test_removeExportFromNonExistentModule :: (?loc :: CallStack) => Test    
test_removeExportFromNonExistentModule = unimplementedTest

test_exportAllNonExistentModule :: (?loc :: CallStack) => Test    
test_exportAllNonExistentModule = unimplementedTest

test_removeNonExistentExport :: (?loc :: CallStack) => Test    
test_removeNonExistentExport = unimplementedTest

test_addExportWithoutDeclaration :: (?loc :: CallStack) => Test    
test_addExportWithoutDeclaration = unimplementedTest
