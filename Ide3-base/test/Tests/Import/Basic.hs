{-# LANGUAGE ImplicitParams #-}
module Tests.Import.Basic (tests_basicImport) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils

tests_basicImport = TestList
    [ test_addImport
    , test_addAndRetrieveImport
    , test_addAndRemoveImport
    , test_addRemoveAndRetrieveImport
    , test_addRemoveAndReAddImport
    ]

test_addImport :: (?loc :: CallStack) => Test    
test_addImport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addImport newProjectInfo newModuleInfo newImport

test_addAndRetrieveImport :: (?loc :: CallStack) => Test
test_addAndRetrieveImport = expectResult newImport $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    importId <- addImport newProjectInfo newModuleInfo newImport
    getImport newProjectInfo newModuleInfo importId

test_addAndRemoveImport :: (?loc :: CallStack) => Test    
test_addAndRemoveImport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    importId <- addImport newProjectInfo newModuleInfo newImport
    removeImport newProjectInfo newModuleInfo importId

test_addRemoveAndRetrieveImport :: (?loc :: CallStack) => Test
test_addRemoveAndRetrieveImport = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    importId <- addImport newProjectInfo newModuleInfo newImport
    removeImport newProjectInfo newModuleInfo importId
    getImport newProjectInfo newModuleInfo importId

test_addRemoveAndReAddImport :: (?loc :: CallStack) => Test
test_addRemoveAndReAddImport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    importId <- addImport newProjectInfo newModuleInfo newImport
    removeImport newProjectInfo newModuleInfo importId
    addImport newProjectInfo newModuleInfo newImport
