{-# LANGUAGE ImplicitParams #-}
module Tests.Export.Basic (tests_basicExport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_basicExport = TestList
    [ test_addExport
    , test_addAndRetreiveExport
    , test_exportAll
    , test_exportNothing
    , test_exportAllThenRemove
    , test_addAndRemoveExport
    , test_addRemoveAndRetreiveExport
    , test_addRemoveAndReaddExport
    ]

test_addExport :: (?loc :: CallStack) => Test
test_addExport = expectSuccess $ do
    createModule newModuleInfo
    addExport newModuleInfo newExport

test_addAndRetreiveExport :: (?loc :: CallStack) => Test
test_addAndRetreiveExport = expectResult newExport $ do
    createModule newModuleInfo
    exportId <- addExport newModuleInfo newExport
    getExport newModuleInfo exportId

test_exportAll :: (?loc :: CallStack) => Test    
test_exportAll = expectSuccess $ do
    createModule newModuleInfo
    exportAll newModuleInfo

test_exportNothing :: (?loc :: CallStack) => Test    
test_exportNothing = expectSuccess $ do
    createModule newModuleInfo
    exportNothing newModuleInfo

test_exportAllThenRemove :: (?loc :: CallStack) => Test    
test_exportAllThenRemove = expectFailure $ do
    createModule newModuleInfo
    exportAll newModuleInfo
    removeExport newModuleInfo nonExistentExportId

test_addAndRemoveExport :: (?loc :: CallStack) => Test    
test_addAndRemoveExport = expectSuccess $ do
    createModule newModuleInfo
    exportId <- addExport newModuleInfo newExport
    removeExport newModuleInfo exportId

test_addRemoveAndRetreiveExport :: (?loc :: CallStack) => Test
test_addRemoveAndRetreiveExport = expectFailure $ do
    createModule newModuleInfo
    exportId <- addExport newModuleInfo newExport
    removeExport newModuleInfo exportId
    getExport newModuleInfo exportId
    
test_addRemoveAndReaddExport :: (?loc :: CallStack) => Test
test_addRemoveAndReaddExport = expectSuccess $ do
    createModule newModuleInfo
    exportId <- addExport newModuleInfo newExport
    removeExport newModuleInfo exportId
    addExport newModuleInfo newExport
