module Tests.Import (tests_import) where

import Test.HUnit

import Tests.Import.NonExistent
import Tests.Import.Basic

tests_import = TestList
    [ tests_nonExistentImport
    , tests_basicImport
    ]
