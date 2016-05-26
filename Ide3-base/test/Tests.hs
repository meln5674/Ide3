module Tests (tests_all) where

import Test.HUnit

import Tests.Module
import Tests.Export
import Tests.Import
import Tests.ImportExport
import Tests.Declaration
import Tests.Parsing

tests_all = TestList
    [ tests_module
    , tests_declaration
    , tests_export
    , tests_import
    , tests_importExport
    , tests_parsing
    ]
