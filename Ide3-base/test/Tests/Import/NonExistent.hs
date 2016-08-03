{-# LANGUAGE ImplicitParams #-}
module Tests.Import.NonExistent (tests_nonExistentImport) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils

tests_nonExistentImport = TestList
    [ test_addImportToNonExistentProject
    , test_addImportToNonExistentModule
    , test_removeNonExistentImport
    ]

test_addImportToNonExistentProject :: (?loc :: CallStack) => Test
test_addImportToNonExistentProject = expectFailure $ do
    addImport nonExistentProjectInfo nonExistentModuleInfo newImport

test_addImportToNonExistentModule :: (?loc :: CallStack) => Test
test_addImportToNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    addImport newProjectInfo nonExistentModuleInfo newImport

test_removeNonExistentImport :: (?loc :: CallStack) => Test    
test_removeNonExistentImport = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeImport newProjectInfo newModuleInfo nonExistentImportId
