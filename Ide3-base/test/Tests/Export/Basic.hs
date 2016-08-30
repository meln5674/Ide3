{-# LANGUAGE ImplicitParams #-}
module Tests.Export.Basic (tests_basicExport) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

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
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addExport newProjectInfo newModuleInfo newExport

test_addAndRetreiveExport :: (?loc :: CallStack) => Test
test_addAndRetreiveExport = expectResult newExport $ do
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

test_addRemoveAndRetreiveExport :: (?loc :: CallStack) => Test
test_addRemoveAndRetreiveExport = expectFailure $ do
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
