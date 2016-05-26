{-# LANGUAGE ImplicitParams #-}
module Tests.Declaration.Basic (tests_basicDeclaration) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils


tests_basicDeclaration = TestList
    [ test_addDeclaration
    , test_addAndRetreiveDeclaration
    , test_addAndRemoveDeclaration
    , test_addRemoveAndRetreiveDeclaration
    , test_addRemoveAndReAddDeclaration
    ]

test_addDeclaration :: (?loc :: CallStack) => Test    
test_addDeclaration = expectSuccess $ do
    createModule newModuleInfo
    addDeclaration newModuleInfo newDeclaration

test_addAndRetreiveDeclaration :: (?loc :: CallStack) => Test    
test_addAndRetreiveDeclaration = expectResult newDeclaration $ do
    createModule newModuleInfo
    addDeclaration newModuleInfo newDeclaration
    getDeclaration newModuleInfo newDeclarationInfo

test_addAndRemoveDeclaration :: (?loc :: CallStack) => Test    
test_addAndRemoveDeclaration = expectSuccess $ do
    createModule newModuleInfo
    addDeclaration newModuleInfo newDeclaration
    removeDeclaration newModuleInfo newDeclarationInfo

test_addRemoveAndRetreiveDeclaration :: (?loc :: CallStack) => Test    
test_addRemoveAndRetreiveDeclaration = expectFailure $ do
    createModule newModuleInfo
    addDeclaration newModuleInfo newDeclaration
    removeDeclaration newModuleInfo newDeclarationInfo
    getDeclaration newModuleInfo newDeclarationInfo

test_addRemoveAndReAddDeclaration :: (?loc :: CallStack) => Test    
test_addRemoveAndReAddDeclaration = expectSuccess $ do
    createModule newModuleInfo
    addDeclaration newModuleInfo newDeclaration
    removeDeclaration newModuleInfo newDeclarationInfo
    addDeclaration newModuleInfo newDeclaration
