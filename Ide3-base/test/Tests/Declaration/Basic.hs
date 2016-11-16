{-# LANGUAGE ImplicitParams #-}
module Tests.Declaration.Basic (tests_basicDeclaration) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils


tests_basicDeclaration = TestList
    [ test_addDeclaration
    , test_addAndRetrieveDeclaration
    , test_addAndRemoveDeclaration
    , test_addRemoveAndRetrieveDeclaration
    , test_addRemoveAndReAddDeclaration
    , test_renameDeclaration
    , test_renameAndRetrieveDeclaration
    ]

test_addDeclaration :: (?loc :: CallStack) => Test    
test_addDeclaration = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration

test_addAndRetrieveDeclaration :: (?loc :: CallStack) => Test    
test_addAndRetrieveDeclaration = expectResult newDeclaration $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    getDeclaration newProjectInfo newModuleInfo newDeclarationInfo

test_addAndRemoveDeclaration :: (?loc :: CallStack) => Test    
test_addAndRemoveDeclaration = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    removeDeclaration newProjectInfo newModuleInfo newDeclarationInfo

test_addRemoveAndRetrieveDeclaration :: (?loc :: CallStack) => Test    
test_addRemoveAndRetrieveDeclaration = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    removeDeclaration newProjectInfo newModuleInfo newDeclarationInfo
    getDeclaration newProjectInfo newModuleInfo newDeclarationInfo

test_addRemoveAndReAddDeclaration :: (?loc :: CallStack) => Test    
test_addRemoveAndReAddDeclaration = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    removeDeclaration newProjectInfo newModuleInfo newDeclarationInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration

test_renameDeclaration :: (?loc :: CallStack) => Test
test_renameDeclaration = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    editDeclaration newProjectInfo newModuleInfo newDeclarationInfo $ const $ return newDeclaration2

test_renameAndRetrieveDeclaration :: (?loc :: CallStack) => Test
test_renameAndRetrieveDeclaration = expectResult newDeclaration2 $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    addDeclaration newProjectInfo newModuleInfo newDeclaration
    editDeclaration newProjectInfo newModuleInfo newDeclarationInfo $ const $ return $ newDeclaration2
    getDeclaration newProjectInfo newModuleInfo newDeclarationInfo2
