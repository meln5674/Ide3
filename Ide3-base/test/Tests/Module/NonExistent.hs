{-# LANGUAGE ImplicitParams #-}
module Tests.Module.NonExistent (tests_nonExistentModule) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad

import Tests.Utils

tests_nonExistentModule = TestList
    [ test_retrieveNonExistentProject
    , test_retrieveNonExistentModule
    , test_removeNonExistentProject
    , test_removeNonExistentModule
    ]

test_retrieveNonExistentProject :: (?loc :: CallStack) => Test    
test_retrieveNonExistentProject = expectFailure $ do
    toFile nonExistentProjectInfo nonExistentModuleInfo

test_retrieveNonExistentModule :: (?loc :: CallStack) => Test    
test_retrieveNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    toFile newProjectInfo nonExistentModuleInfo


test_removeNonExistentProject :: (?loc :: CallStack) => Test    
test_removeNonExistentProject = expectFailure $ do
    removeModule nonExistentProjectInfo nonExistentModuleInfo

test_removeNonExistentModule :: (?loc :: CallStack) => Test    
test_removeNonExistentModule = expectFailure $ do
    addProject newProjectInfo
    removeModule newProjectInfo nonExistentModuleInfo


