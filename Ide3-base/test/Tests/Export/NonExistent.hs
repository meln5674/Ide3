{-# LANGUAGE ImplicitParams #-}
module Tests.Export.NonExistent (tests_nonExistentExport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_nonExistentExport = TestList
    [ test_addExportToNonExistentProject
    , test_addExportToNonExistentModule
    , test_removeExportFromNonExistentProject
    , test_removeExportFromNonExistentModule
    , test_exportAllNonExistentProject
    , test_exportAllNonExistentModule
    , test_removeNonExistentExport
    ]

test_addExportToNonExistentProject :: (?loc :: CallStack) => Test    
test_addExportToNonExistentProject = expectFailure $ do
    addExport nonExistentProjectInfo nonExistentModuleInfo newExport

test_addExportToNonExistentModule :: (?loc :: CallStack) => Test    
test_addExportToNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    addExport newProjectInfo nonExistentModuleInfo newExport

test_removeExportFromNonExistentProject :: (?loc :: CallStack) => Test    
test_removeExportFromNonExistentProject = expectFailure $ do
    removeExport nonExistentProjectInfo nonExistentModuleInfo nonExistentExportId

test_removeExportFromNonExistentModule :: (?loc :: CallStack) => Test    
test_removeExportFromNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    removeExport newProjectInfo nonExistentModuleInfo nonExistentExportId

test_exportAllNonExistentProject :: (?loc :: CallStack) => Test    
test_exportAllNonExistentProject = expectFailure $ do
    exportAll nonExistentProjectInfo nonExistentModuleInfo

test_exportAllNonExistentModule :: (?loc :: CallStack) => Test    
test_exportAllNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    exportAll newProjectInfo nonExistentModuleInfo

test_removeNonExistentExport :: (?loc :: CallStack) => Test    
test_removeNonExistentExport = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeExport newProjectInfo newModuleInfo nonExistentExportId
