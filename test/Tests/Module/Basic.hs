{-# LANGUAGE ImplicitParams #-}
module Tests.Module.Basic (tests_basicModule) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad
import qualified Ide3.Module as Module

import Tests.Utils

tests_basicModule = TestList
    [ test_addModule
    , test_addAndRetreiveModule
    , test_addAndRemoveModule
    , test_addRemoveAndRetreiveModule
    , test_addRemoveAndReAddModule
    ]

test_addModule :: (?loc :: CallStack) => Test    
test_addModule = expectSuccess $ createModule newModuleInfo

test_addAndRetreiveModule :: (?loc :: CallStack) => Test    
test_addAndRetreiveModule = expectResult (Module.new newModuleInfo) $ do
    createModule newModuleInfo
    getModule newModuleInfo

test_addAndRemoveModule :: (?loc :: CallStack) => Test    
test_addAndRemoveModule = expectSuccess $ do
    createModule newModuleInfo
    removeModule newModuleInfo

test_addRemoveAndRetreiveModule :: (?loc :: CallStack) => Test    
test_addRemoveAndRetreiveModule = expectFailure $ do
    createModule newModuleInfo
    removeModule newModuleInfo
    getModule newModuleInfo

test_addRemoveAndReAddModule :: (?loc :: CallStack) => Test    
test_addRemoveAndReAddModule = expectSuccess $ do
    createModule newModuleInfo
    removeModule newModuleInfo
    createModule newModuleInfo

