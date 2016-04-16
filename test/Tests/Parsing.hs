module Tests.Parsing (tests_parsing) where

import Test.HUnit

import Tests.Parsing.Module
import Tests.Parsing.Import
import Tests.Parsing.Export
import Tests.Parsing.Declaration

tests_parsing = TestList
    [ tests_parsingDeclaration
    , tests_parsingImport
    , tests_parsingExport
    , tests_parsingModule
    ]
