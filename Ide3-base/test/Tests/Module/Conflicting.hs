{-# LANGUAGE ImplicitParams #-}
module Tests.Module.Conflicting (tests_conflictingModule) where

import GHC.Stack

import Test.HUnit

import qualified Ide3.Module as Module
import Ide3.Monad

import Tests.Utils

tests_conflictingModule = TestList
    [ test_add2RemoveRetreiveModule
    , test_add2RemoveRetreiveBadModule
    , test_addDuplicateModule
    ]

test_add2RemoveRetreiveModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetreiveModule = expectResult (Module.new newModuleInfo) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    removeModule newProjectInfo newModuleInfo2
    getModule newProjectInfo newModuleInfo

test_add2RemoveRetreiveBadModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetreiveBadModule = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    removeModule newProjectInfo newModuleInfo
    getModule newProjectInfo newModuleInfo

test_addDuplicateModule :: (?loc :: CallStack) => Test    
test_addDuplicateModule = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo
