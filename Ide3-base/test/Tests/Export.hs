module Tests.Export (tests_export) where

import Test.HUnit

import Tests.Export.NonExistent
import Tests.Export.Basic

tests_export = TestList
    [ tests_nonExistentExport
    , tests_basicExport
    ]
