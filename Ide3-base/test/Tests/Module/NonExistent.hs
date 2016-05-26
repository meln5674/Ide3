{-# LANGUAGE ImplicitParams #-}
module Tests.Module.NonExistent (tests_nonExistentModule) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_nonExistentModule = TestList
    [ test_retrieveNonExistentModule
    , test_removeNonExistentModule
    ]

test_retrieveNonExistentModule :: (?loc :: CallStack) => Test    
test_retrieveNonExistentModule = expectFailure $ getModule nonExistentModuleInfo

test_removeNonExistentModule :: (?loc :: CallStack) => Test    
test_removeNonExistentModule = expectFailure $ removeModule nonExistentModuleInfo


