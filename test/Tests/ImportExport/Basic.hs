{-# LANGUAGE ImplicitParams #-}
module Tests.ImportExport.Basic (tests_basicImportExport) where

import GHC.Stack

import Test.HUnit

import Tests.Utils

tests_basicImportExport = TestList
    [ test_importNonExistentSymbol
    , test_importSymbol
    , test_importUnexportedSymbol
    , test_nonExportedSymbolNotVisible
    , test_importCompoundSymbol
    , test_partialImportCompoundSymbol
    , test_partialExportCompoundSymbol
    , test_partialImportExportCompoundSymbol
    ]

test_importNonExistentSymbol :: (?loc :: CallStack) => Test    
test_importNonExistentSymbol = unimplementedTest

test_importSymbol :: (?loc :: CallStack) => Test    
test_importSymbol = unimplementedTest

test_importUnexportedSymbol :: (?loc :: CallStack) => Test    
test_importUnexportedSymbol = unimplementedTest

test_nonExportedSymbolNotVisible :: (?loc :: CallStack) => Test    
test_nonExportedSymbolNotVisible = unimplementedTest

test_importCompoundSymbol :: (?loc :: CallStack) => Test    
test_importCompoundSymbol = unimplementedTest

test_partialImportCompoundSymbol :: (?loc :: CallStack) => Test    
test_partialImportCompoundSymbol = unimplementedTest

test_partialExportCompoundSymbol :: (?loc :: CallStack) => Test    
test_partialExportCompoundSymbol = unimplementedTest

test_partialImportExportCompoundSymbol :: (?loc :: CallStack) => Test    
test_partialImportExportCompoundSymbol = unimplementedTest
