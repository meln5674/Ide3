{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Mechanism where

import Control.Monad.Trans.Except
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
    Left msg -> error $ "Failed to parse import: " ++ msg
addRawExport :: ProjectM m => ModuleInfo -> String -> m ()
addRawExport mi s = case Export.parse s of
    Right e -> addExport mi (WithBody e s)
    Left msg -> error $ "Failed to parse import" ++ msg
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> m ()
addRawDeclaration i s = case Declaration.parse s of
    Right d -> addDeclaration i (WithBody d s)
    Left msg -> error $ "Failed to parse declaration" ++ msg

getExternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getExternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.exportedSymbols >>= return . map getChild

getInternalSymbols :: ProjectM m => ModuleInfo -> m (Either ProjectError [Symbol])
getInternalSymbols m = runExceptT $ ExceptT (getModule m) >>= Module.internalSymbols

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
            Right p' -> p'
            Left msg -> error $ "Failed to add declaration: " ++ msg
    addImport mi i = modify $ \p ->
        case Project.addImport p mi i of
            Right p' -> p'
            Left msg -> error $ "Failed to add import: " ++ msg
    removeImport mi i = modify $ \p ->
        case Project.removeImport p mi i of
            Right p' -> p'
            Left msg -> error $ "Failed to remove import: " ++ msg
    addExport mi e = modify $ \p ->
        case Project.addExport p mi e of
            Right p' -> p'
            Left msg -> error $ "Failed to add export: " ++ msg
    removeExport mi e = modify $ \p ->
        case Project.removeExport p mi e of
            Right p' -> p'
            Left msg -> error $ "Failed to remove export" ++ msg
    exportAll mi = modify $ \p ->
        case Project.exportAll p mi of
            Right p' -> p'
            Left msg -> error $ "Failed to export all: " ++ msg
    

