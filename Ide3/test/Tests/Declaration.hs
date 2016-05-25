module Tests.Declaration (tests_declaration) where

import Test.HUnit

import Tests.Declaration.NonExistent
import Tests.Declaration.Basic

tests_declaration = TestList
    [ tests_nonExistentDeclaration
    , tests_basicDeclaration
    ]
