{-# LANGUAGE ImplicitParams #-}
module Tests.Declaration.NonExistent (tests_nonExistentDeclaration) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils

tests_nonExistentDeclaration :: Test
tests_nonExistentDeclaration = TestList
    [ test_addDeclarationToNonExistentProject
    , test_addDeclarationToNonExistentModule
    , test_removeDeclarationFromNonExistentProject
    , test_removeDeclarationFromNonExistentModule
    , test_editDeclarationInNonExistentProject
    , test_editDeclarationInNonExistentModule
    , test_removeNonExistentDeclaration
    , test_editNonExistentDeclaration
    ]

test_addDeclarationToNonExistentProject :: (?loc :: CallStack) => Test
test_addDeclarationToNonExistentProject = expectFailure $ addDeclaration nonExistentProjectInfo nonExistentModuleInfo newDeclaration

test_addDeclarationToNonExistentModule :: (?loc :: CallStack) => Test    
test_addDeclarationToNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    addDeclaration newProjectInfo nonExistentModuleInfo newDeclaration

test_removeDeclarationFromNonExistentProject :: (?loc :: CallStack) => Test    
test_removeDeclarationFromNonExistentProject = expectFailure $ removeDeclaration nonExistentProjectInfo nonExistentModuleInfo newDeclarationInfo

test_removeDeclarationFromNonExistentModule :: (?loc :: CallStack) => Test    
test_removeDeclarationFromNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    removeDeclaration newProjectInfo nonExistentModuleInfo newDeclarationInfo

test_editDeclarationInNonExistentProject :: (?loc :: CallStack) => Test    
test_editDeclarationInNonExistentProject = expectFailure $ do
    editDeclaration nonExistentProjectInfo nonExistentModuleInfo newDeclarationInfo undefined

test_editDeclarationInNonExistentModule :: (?loc :: CallStack) => Test    
test_editDeclarationInNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    editDeclaration newProjectInfo nonExistentModuleInfo newDeclarationInfo undefined

test_removeNonExistentDeclaration :: (?loc :: CallStack) => Test    
test_removeNonExistentDeclaration = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeDeclaration newProjectInfo newModuleInfo nonExistentDeclarationInfo

test_editNonExistentDeclaration :: (?loc :: CallStack) => Test    
test_editNonExistentDeclaration = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    editDeclaration newProjectInfo newModuleInfo nonExistentDeclarationInfo undefined
