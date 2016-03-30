{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Mechanism where

import Control.Monad.Identity
import Control.Monad.State

import Ide3.Types
import qualified Ide3.Project as Project
import qualified Ide3.Module as Module
import qualified Ide3.Import as Import
import qualified Ide3.Export as Export
import qualified Ide3.Declaration as Declaration

import Ide3.Monad
        

addRawImport :: ProjectM m => ModuleInfo -> String -> m ()
addRawImport mi s = case Import.parse s of
    Right i -> addImport mi (WithBody i s)
    Left _ -> error "Failed to parse import"
addRawExport :: ProjectM m => ModuleInfo -> String -> m ()
addRawExport mi s = case Export.parse s of
    Right e -> addExport mi (WithBody e s)
    Left _ -> error "Failed to parse import"


addRawDeclaration :: ProjectM m => ModuleInfo -> String -> m ()
addRawDeclaration i s = case Declaration.parse s of
    Right d -> addDeclaration i (WithBody d s)
    Left _ -> error "Failed to parse declaration"


type ProjectState = State Project

instance ProjectM ProjectState where
    load = error "Cannot load a test project"
    new i = put $ Project.new i
    finalize = error "Cannot finalize a test project"
    editProjectInfo f = modify $ \(Project i ms b) -> (Project (f i) ms b)
    addModule m = modify $ \p -> Project.addModule p m
    createModule i = modify $ \p -> Project.createModule p i
    getModule i = gets $ \p -> Project.getModule p i
    addDeclaration i d = modify $ \p ->
        case Project.addDeclaration p i d of
            Just p' -> p'
            Nothing -> error "Failed to add declaration"
    addImport mi i = modify $ \p ->
        case Project.addImport p mi i of
            Just p' -> p'
            Nothing -> error "Failed to add import"
    removeImport mi i = modify $ \p ->
        case Project.removeImport p mi i of
            Just p' -> p'
            Nothing -> error "Failed to remove import"
    addExport mi e = modify $ \p ->
        case Project.addExport p mi e of
            Just p' -> p'
            Nothing -> error "Failed to add export"
    removeExport mi e = modify $ \p ->
        case Project.removeExport p mi e of
            Just p' -> p'
            Nothing -> error "Failed to remove export"
    exportAll mi = modify $ \p ->
        case Project.exportAll p mi of
            Just p' -> p'
            Nothing -> error "Failed to export all"
    

testModuleInfo = ModuleInfo (Symbol "Test")

test = let run = new ProjectInfo
                    >> editProjectInfo id 
                    >> createModule
                        testModuleInfo
                    >> addRawDeclaration
                        testModuleInfo
                        "type Test = String"
                    >> addRawImport
                        testModuleInfo
                        "import Data.List"
                    >> getModule
                        testModuleInfo
       in Module.toFile <$> evalState run Project.empty


testIO = case test of
    Just x -> putStr (x++"\n")
    Nothing -> print "FAILED"
