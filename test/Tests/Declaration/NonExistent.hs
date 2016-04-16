{-# LANGUAGE ImplicitParams #-}
module Tests.Declaration.NonExistent (tests_nonExistentDeclaration) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_nonExistentDeclaration = TestList
    [ test_addDeclarationToNonExistentModule
    , test_removeDeclarationFromNonExistentModule
    , test_editDeclarationInNonExistentModule
    , test_removeNonExistentDeclaration
    , test_editNonExistentDeclaration
    ]

test_addDeclarationToNonExistentModule :: (?loc :: CallStack) => Test    
test_addDeclarationToNonExistentModule = expectFailure $ addDeclaration nonExistentModuleInfo newDeclaration

test_removeDeclarationFromNonExistentModule :: (?loc :: CallStack) => Test    
test_removeDeclarationFromNonExistentModule = expectFailure $ removeDeclaration nonExistentModuleInfo newDeclarationInfo

test_editDeclarationInNonExistentModule :: (?loc :: CallStack) => Test    
test_editDeclarationInNonExistentModule = expectFailure $ editDeclaration nonExistentModuleInfo newDeclarationInfo Right

test_removeNonExistentDeclaration :: (?loc :: CallStack) => Test    
test_removeNonExistentDeclaration = expectFailure $ do
    createModule newModuleInfo
    removeDeclaration newModuleInfo nonExistentDeclarationInfo

test_editNonExistentDeclaration :: (?loc :: CallStack) => Test    
test_editNonExistentDeclaration = expectFailure $ do
    createModule newModuleInfo
    editDeclaration newModuleInfo nonExistentDeclarationInfo Right
