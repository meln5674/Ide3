{-|
Module      : Ide3.Mechanism.Except
Description : Exception handling wrapper
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides matching functions to those provided by 'Ide3.Monad' and 
'Ide3.Mechanism.Internal', with each function operating in the ExceptT monad,
allowing for seamless error throwing and handling with throwE and catchE.
-}
{-
module Ide3.Mechanism.Except where

import Control.Monad.Trans.Except
import Control.Monad.Trans

import Ide3.Types
import Ide3.Monad (ProjectM)
import qualified Ide3.Monad as I
import qualified Ide3.Mechanism.Internal as I

load :: ProjectM m => ExceptT ProjectError m ()
load = ExceptT I.load
new :: ProjectM m => ProjectInfo -> ExceptT ProjectError m ()
new = ExceptT . I.new 
finalize :: ProjectM m => ExceptT ProjectError m ()
finalize = ExceptT I.finalize
editProjectInfo :: ProjectM m => (ProjectInfo -> ProjectInfo) -> ExceptT ProjectError m ()
editProjectInfo = ExceptT . I.editProjectInfo
addModule :: ProjectM m => Module -> ExceptT ProjectError m ()
addModule = ExceptT . I.addModule
createModule:: ProjectM m =>  ModuleInfo -> ExceptT ProjectError m ()
createModule = ExceptT . I.createModule
getModule :: ProjectM m =>  ModuleInfo -> ExceptT ProjectError m Module
getModule = ExceptT . I.getModule
removeModule :: ProjectM m =>  ModuleInfo -> ExceptT ProjectError m ()
removeModule = ExceptT . I.removeModule
addDeclaration :: ProjectM m =>  ModuleInfo -> WithBody Declaration -> ExceptT ProjectError m ()
addDeclaration x = ExceptT . I.addDeclaration x
addImport :: ProjectM m =>  ModuleInfo -> WithBody Import -> ExceptT ProjectError m ImportId
addImport x = ExceptT . I.addImport x
removeImport :: ProjectM m =>  ModuleInfo -> ImportId -> ExceptT ProjectError m ()
removeImport x = ExceptT . I.removeImport x
addExport :: ProjectM m =>  ModuleInfo -> WithBody Export -> ExceptT ProjectError m ExportId
addExport x = ExceptT . I.addExport x
removeExport :: ProjectM m =>  ModuleInfo -> ExportId -> ExceptT ProjectError m ()
removeExport x = ExceptT . I.removeExport x
exportAll :: ProjectM m =>  ModuleInfo -> ExceptT ProjectError m ()
exportAll = ExceptT . I.exportAll
getModules :: ProjectM m => ExceptT ProjectError m [ModuleInfo]
getModules = ExceptT I.getModules


addRawImport:: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ImportId
addRawImport x = ExceptT . I.addRawImport x
addRawExport :: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ExportId
addRawExport x  = ExceptT . I.addRawExport x
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ()
addRawDeclaration x  = ExceptT . I.addRawDeclaration x
addRawModule :: ProjectM m => String -> Maybe FilePath -> ExceptT ProjectError m ModuleInfo
addRawModule x = ExceptT . I.addRawModule x
getExternalSymbols :: ProjectM m => ModuleInfo -> ExceptT ProjectError m [Symbol]
getExternalSymbols = ExceptT . I.getExternalSymbols
getInternalSymbols :: ProjectM m => ModuleInfo -> ExceptT ProjectError m [Symbol]
getInternalSymbols = ExceptT . I.getInternalSymbols
-}
