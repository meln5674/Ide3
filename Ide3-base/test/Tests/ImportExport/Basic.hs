{-# LANGUAGE ImplicitParams #-}
module Tests.ImportExport.Basic (tests_basicImportExport) where

import GHC.Stack

import Test.HUnit

import Ide3.NewMonad
import Ide3.NewMonad.Utils

import qualified Ide3.Module as Module

import Tests.Utils

tests_basicImportExport :: Test
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
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addExport newProjectInfo newModuleInfo2 nonExistentDeclarationExport
    addImport newProjectInfo newModuleInfo newModule2Import
    Module.internalSymbols' newProjectInfo newModuleInfo

test_importSymbol :: (?loc :: CallStack) => Test
test_importSymbol = expectPredicate (newDeclarationSymbol `elem`) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newDeclaration
    addExport newProjectInfo newModuleInfo2 newDeclarationExport
    addImport newProjectInfo newModuleInfo newModule2Import
    Module.internalSymbols' newProjectInfo newModuleInfo

test_importUnexportedSymbol :: (?loc :: CallStack) => Test
test_importUnexportedSymbol = expectFailure $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newDeclaration
    exportNothing newProjectInfo newModuleInfo2
    addImport newProjectInfo newModuleInfo newDeclarationImport
    Module.internalSymbols' newProjectInfo newModuleInfo
    
test_nonExportedSymbolNotVisible :: (?loc :: CallStack) => Test 
test_nonExportedSymbolNotVisible = expectPredicate (not . (newDeclarationSymbol `elem`)) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newDeclaration
    exportNothing newProjectInfo newModuleInfo2
    addImport newProjectInfo newModuleInfo newModule2Import
    Module.internalSymbols' newProjectInfo newModuleInfo

test_importCompoundSymbol :: (?loc :: CallStack) => Test
test_importCompoundSymbol = expectPredicate (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationSymbols) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newCompoundDeclaration
    addImport newProjectInfo newModuleInfo newModule2Import
    Module.internalSymbols' newProjectInfo newModuleInfo

test_partialImportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialImportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newCompoundDeclaration
    addImport newProjectInfo newModuleInfo newCompoundDeclarationPartialImport
    Module.internalSymbols' newProjectInfo newModuleInfo
                    

test_partialExportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialExportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newCompoundDeclaration
    addExport newProjectInfo newModuleInfo2 newCompoundDeclarationPartialExport
    addImport newProjectInfo newModuleInfo newModule2Import
    Module.internalSymbols' newProjectInfo newModuleInfo

test_partialImportExportCompoundSymbol :: (?loc :: CallStack) => Test
test_partialImportExportCompoundSymbol = expectPredicate
    (\ss -> all (\s -> s `elem` ss) newCompoundDeclarationIncluded 
         && all (\s -> not $ s `elem` ss) newCompoundDeclarationExcluded
         && newCompoundDeclarationSymbol `elem` ss) $ do
    addProject newProjectInfo
    createModule newProjectInfo newModuleInfo
    createModule newProjectInfo newModuleInfo2
    addDeclaration newProjectInfo newModuleInfo2 newCompoundDeclaration
    addExport newProjectInfo newModuleInfo2 newCompoundDeclarationPartialExport
    addImport newProjectInfo newModuleInfo newCompoundDeclarationPartialImport
    Module.internalSymbols' newProjectInfo newModuleInfo
