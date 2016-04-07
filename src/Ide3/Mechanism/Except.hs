module Ide3.Mechanism.Except where

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Monad (ProjectM)
import qualified Ide3.Monad as I
import qualified Ide3.Mechanism.Internal as I

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



addRawImport:: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ImportId
addRawImport x  = ExceptT . I.addRawImport x
addRawExport :: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ExportId
addRawExport x  = ExceptT . I.addRawExport x
addRawDeclaration :: ProjectM m => ModuleInfo -> String -> ExceptT ProjectError m ()
addRawDeclaration x  = ExceptT . I.addRawDeclaration x
addRawModule :: ProjectM m => String -> ExceptT ProjectError m ModuleInfo
addRawModule = ExceptT . I.addRawModule
getExternalSymbols :: ProjectM m => ModuleInfo -> ExceptT ProjectError m [Symbol]
getExternalSymbols = ExceptT . I.getExternalSymbols
getInternalSymbols :: ProjectM m => ModuleInfo -> ExceptT ProjectError m [Symbol]
getInternalSymbols = ExceptT . I.getInternalSymbols


