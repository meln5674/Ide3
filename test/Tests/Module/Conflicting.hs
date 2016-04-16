{-# LANGUAGE ImplicitParams #-}
module Tests.Module.Conflicting (tests_conflictingModule) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad

import Tests.Utils

tests_conflictingModule = TestList
    [ test_add2RemoveRetreiveModule
    , test_add2RemoveRetreiveBadModule
    , test_addDuplicateModule
    ]

test_add2RemoveRetreiveModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetreiveModule = unimplementedTest

test_add2RemoveRetreiveBadModule :: (?loc :: CallStack) => Test    
test_add2RemoveRetreiveBadModule = unimplementedTest

test_addDuplicateModule :: (?loc :: CallStack) => Test    
test_addDuplicateModule = unimplementedTest
