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
    ]

test_addExportToNonExistentModule :: (?loc :: CallStack) => Test    
test_addExportToNonExistentModule = expectFailure $ do
    addExport nonExistentModuleInfo newExport

test_removeExportFromNonExistentModule :: (?loc :: CallStack) => Test    
test_removeExportFromNonExistentModule = expectFailure $ do
    removeExport nonExistentModuleInfo nonExistentExportId

test_exportAllNonExistentModule :: (?loc :: CallStack) => Test    
test_exportAllNonExistentModule = expectFailure $ do
    exportAll nonExistentModuleInfo

test_removeNonExistentExport :: (?loc :: CallStack) => Test    
test_removeNonExistentExport = expectFailure $ do
    createModule newModuleInfo
    removeExport newModuleInfo nonExistentExportId
