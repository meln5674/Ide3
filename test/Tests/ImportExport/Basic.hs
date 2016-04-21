{-# LANGUAGE ImplicitParams #-}
module Tests.ImportExport.Basic (tests_basicImportExport) where

import GHC.Stack

import Test.HUnit

import Ide3.Monad
import Ide3.Mechanism

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
test_importNonExistentSymbol = expectFailure $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addExport newModuleInfo2 nonExistentDeclarationExport
    addImport newModuleInfo newModule2Import
    getInternalSymbols newModuleInfo

test_importSymbol :: (?loc :: CallStack) => Test
test_importSymbol = expectPredicate (newDeclarationSymbol `elem`) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newDeclaration
    addExport newModuleInfo2 newDeclarationExport
    addImport newModuleInfo newModule2Import
    getInternalSymbols newModuleInfo

test_importUnexportedSymbol :: (?loc :: CallStack) => Test
test_importUnexportedSymbol = expectFailure $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newDeclaration
    exportNothing newModuleInfo2
    addImport newModuleInfo newDeclarationImport
    getInternalSymbols newModuleInfo
    
test_nonExportedSymbolNotVisible :: (?loc :: CallStack) => Test 
test_nonExportedSymbolNotVisible = expectPredicate (not . (newDeclarationSymbol `elem`)) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newDeclaration
    exportNothing newModuleInfo2
    addImport newModuleInfo newModule2Import
    getInternalSymbols newModuleInfo

test_importCompoundSymbol :: (?loc :: CallStack) => Test
test_importCompoundSymbol = expectPredicate (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationSymbols) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newCompoundDeclaration
    addImport newModuleInfo newModule2Import
    getInternalSymbols newModuleInfo

test_partialImportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialImportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newCompoundDeclaration
    addImport newModuleInfo newCompoundDeclarationPartialImport
    getInternalSymbols newModuleInfo
                    

test_partialExportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialExportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newCompoundDeclaration
    addExport newModuleInfo2 newCompoundDeclarationPartialExport
    addImport newModuleInfo newModule2Import
    getInternalSymbols newModuleInfo

test_partialImportExportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialImportExportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    createModule newModuleInfo
    createModule newModuleInfo2
    addDeclaration newModuleInfo2 newCompoundDeclaration
    addExport newModuleInfo2 newCompoundDeclarationPartialExport
    addImport newModuleInfo newCompoundDeclarationPartialImport
    getInternalSymbols newModuleInfo
