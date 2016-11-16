{-# LANGUAGE ImplicitParams #-}
module Tests.Export.Basic (tests_basicExport) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils

tests_basicExport = TestList
    [ test_addExport
    , test_addAndRetrieveExport
    , test_exportAll
    , test_exportNothing
    , test_exportAllThenRemove
    , test_addAndRemoveExport
    , test_addRemoveAndRetrieveExport
    , test_addRemoveAndReaddExport
    ]

test_addExport :: (?loc :: CallStack) => Test
test_addExport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addExport newProjectInfo newModuleInfo newExport

test_addAndRetrieveExport :: (?loc :: CallStack) => Test
test_addAndRetrieveExport = expectResult newExport $ do
    addProject newProjectInfo
    createModule newProjectInfo  newModuleInfo
    exportId <- addExport newProjectInfo newModuleInfo newExport
    getExport newProjectInfo newModuleInfo exportId

test_exportAll :: (?loc :: CallStack) => Test    
test_exportAll = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportAll newProjectInfo newModuleInfo

test_exportNothing :: (?loc :: CallStack) => Test    
test_exportNothing = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportNothing newProjectInfo newModuleInfo

test_exportAllThenRemove :: (?loc :: CallStack) => Test    
test_exportAllThenRemove = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportAll newProjectInfo newModuleInfo
    removeExport newProjectInfo newModuleInfo nonExistentExportId

test_addAndRemoveExport :: (?loc :: CallStack) => Test    
test_addAndRemoveExport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportId <- addExport newProjectInfo newModuleInfo newExport
    removeExport newProjectInfo newModuleInfo exportId

test_addRemoveAndRetrieveExport :: (?loc :: CallStack) => Test
test_addRemoveAndRetrieveExport = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportId <- addExport newProjectInfo newModuleInfo newExport
    removeExport newProjectInfo newModuleInfo exportId
    getExport newProjectInfo newModuleInfo exportId
    
test_addRemoveAndReaddExport :: (?loc :: CallStack) => Test
test_addRemoveAndReaddExport = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    exportId <- addExport newProjectInfo newModuleInfo newExport
    removeExport newProjectInfo newModuleInfo exportId
    addExport newProjectInfo newModuleInfo newExport
