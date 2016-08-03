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
import Ide3.Monad (SolutionM)
import qualified Ide3.Monad as I
import qualified Ide3.Mechanism.Internal as I

load :: SolutionM m => ExceptT SolutionError m ()
load = ExceptT I.load
new :: SolutionM m => ProjectInfo -> ExceptT SolutionError m ()
new = ExceptT . I.new 
finalize :: SolutionM m => ExceptT SolutionError m ()
finalize = ExceptT I.finalize
editProjectInfo :: SolutionM m => (ProjectInfo -> ProjectInfo) -> ExceptT SolutionError m ()
editProjectInfo = ExceptT . I.editProjectInfo
addModule :: SolutionM m => Module -> ExceptT SolutionError m ()
addModule = ExceptT . I.addModule
createModule:: SolutionM m =>  ModuleInfo -> ExceptT SolutionError m ()
createModule = ExceptT . I.createModule
getModule :: SolutionM m =>  ModuleInfo -> ExceptT SolutionError m Module
getModule = ExceptT . I.getModule
removeModule :: SolutionM m =>  ModuleInfo -> ExceptT SolutionError m ()
removeModule = ExceptT . I.removeModule
addDeclaration :: SolutionM m =>  ModuleInfo -> WithBody Declaration -> ExceptT SolutionError m ()
addDeclaration x = ExceptT . I.addDeclaration x
addImport :: SolutionM m =>  ModuleInfo -> WithBody Import -> ExceptT SolutionError m ImportId
addImport x = ExceptT . I.addImport x
removeImport :: SolutionM m =>  ModuleInfo -> ImportId -> ExceptT SolutionError m ()
removeImport x = ExceptT . I.removeImport x
addExport :: SolutionM m =>  ModuleInfo -> WithBody Export -> ExceptT SolutionError m ExportId
addExport x = ExceptT . I.addExport x
removeExport :: SolutionM m =>  ModuleInfo -> ExportId -> ExceptT SolutionError m ()
removeExport x = ExceptT . I.removeExport x
exportAll :: SolutionM m =>  ModuleInfo -> ExceptT SolutionError m ()
exportAll = ExceptT . I.exportAll
getModules :: SolutionM m => ExceptT SolutionError m [ModuleInfo]
getModules = ExceptT I.getModules


addRawImport:: SolutionM m => ModuleInfo -> String -> ExceptT SolutionError m ImportId
addRawImport x = ExceptT . I.addRawImport x
addRawExport :: SolutionM m => ModuleInfo -> String -> ExceptT SolutionError m ExportId
addRawExport x  = ExceptT . I.addRawExport x
addRawDeclaration :: SolutionM m => ModuleInfo -> String -> ExceptT SolutionError m ()
addRawDeclaration x  = ExceptT . I.addRawDeclaration x
addRawModule :: SolutionM m => String -> Maybe FilePath -> ExceptT SolutionError m ModuleInfo
addRawModule x = ExceptT . I.addRawModule x
getExternalSymbols :: SolutionM m => ModuleInfo -> ExceptT SolutionError m [Symbol]
getExternalSymbols = ExceptT . I.getExternalSymbols
getInternalSymbols :: SolutionM m => ModuleInfo -> ExceptT SolutionError m [Symbol]
getInternalSymbols = ExceptT . I.getInternalSymbols
-}
