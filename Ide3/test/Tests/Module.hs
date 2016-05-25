module Tests.Module (tests_module) where

import Test.HUnit

import Tests.Module.NonExistent
import Tests.Module.Basic
import Tests.Module.Conflicting

tests_module = TestList
    [ tests_nonExistentModule
    , tests_basicModule
    , tests_conflictingModule
    ]
