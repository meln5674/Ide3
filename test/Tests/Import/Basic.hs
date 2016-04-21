{-# LANGUAGE ImplicitParams #-}
module Tests.Import.Basic (tests_basicImport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_basicImport = TestList
    [ test_addImport
    , test_addAndRetreiveImport
    , test_addAndRemoveImport
    , test_addRemoveAndRetreiveImport
    , test_addRemoveAndReAddImport
    ]

test_addImport :: (?loc :: CallStack) => Test    
test_addImport = expectSuccess $ do
    createModule newModuleInfo
    addImport newModuleInfo newImport

test_addAndRetreiveImport :: (?loc :: CallStack) => Test
test_addAndRetreiveImport = expectResult newImport $ do
    createModule newModuleInfo
    importId <- addImport newModuleInfo newImport
    getImport newModuleInfo importId

test_addAndRemoveImport :: (?loc :: CallStack) => Test    
test_addAndRemoveImport = expectSuccess $ do
    createModule newModuleInfo
    importId <- addImport newModuleInfo newImport
    removeImport newModuleInfo importId

test_addRemoveAndRetreiveImport :: (?loc :: CallStack) => Test
test_addRemoveAndRetreiveImport = expectFailure $ do
    createModule newModuleInfo
    importId <- addImport newModuleInfo newImport
    removeImport newModuleInfo importId
    getImport newModuleInfo importId

test_addRemoveAndReAddImport :: (?loc :: CallStack) => Test
test_addRemoveAndReAddImport = expectSuccess $ do
    createModule newModuleInfo
    importId <- addImport newModuleInfo newImport
    removeImport newModuleInfo importId
    addImport newModuleInfo newImport
