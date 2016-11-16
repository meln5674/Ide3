{-# LANGUAGE ImplicitParams #-}
module Tests.Module.Conflicting (tests_conflictingModule) where

import GHC.Stack

import Test.HUnit

import qualified Ide3.Module as Module
import Ide3.NewMonad

import Tests.Utils

tests_conflictingModule = TestList
    [ test_add2RemoveRetrieveModule
    , test_add2RemoveRetrieveBadModule
    , test_addDuplicateModule
    ]


test_add2RemoveRetrieveModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetrieveModule = expectResult (Module.toFile $ Module.new newModuleInfo) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    removeModule newProjectInfo newModuleInfo2
    toFile newProjectInfo newModuleInfo

test_add2RemoveRetrieveBadModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetrieveBadModule = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    removeModule newProjectInfo newModuleInfo
    toFile newProjectInfo newModuleInfo


test_addDuplicateModule :: (?loc :: CallStack) => Test    
test_addDuplicateModule = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo
