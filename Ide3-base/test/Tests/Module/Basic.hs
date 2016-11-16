{-# LANGUAGE ImplicitParams #-}
module Tests.Module.Basic (tests_basicModule) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad
import qualified Ide3.Module as Module

import Tests.Utils

tests_basicModule = TestList
    [ test_addModule
    , test_addAndRetrieveModule
    , test_addAndRemoveModule
    , test_addRemoveAndRetrieveModule
    , test_addRemoveAndReAddModule
    ]

test_addModule :: (?loc :: CallStack) => Test    
test_addModule = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo

test_addAndRetrieveModule :: (?loc :: CallStack) => Test    
test_addAndRetrieveModule = expectResult (Module.toFile $ Module.new newModuleInfo) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    toFile newProjectInfo newModuleInfo

test_addAndRemoveModule :: (?loc :: CallStack) => Test    
test_addAndRemoveModule = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeModule newProjectInfo newModuleInfo

test_addRemoveAndRetrieveModule :: (?loc :: CallStack) => Test    
test_addRemoveAndRetrieveModule = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeModule newProjectInfo newModuleInfo
    toFile newProjectInfo newModuleInfo

test_addRemoveAndReAddModule :: (?loc :: CallStack) => Test    
test_addRemoveAndReAddModule = expectSuccess $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    removeModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo

