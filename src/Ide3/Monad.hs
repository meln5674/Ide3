module Ide3.Monad where

import Ide3.Types

class Monad m => ProjectM m where
    load :: m ()
    new :: ProjectInfo -> m ()
    finalize :: m ()
    editProjectInfo :: (ProjectInfo -> ProjectInfo) -> m ()
    addModule :: Module -> m (Either ProjectError ())
    createModule :: ModuleInfo -> m (Either ProjectError ())
    getModule :: ModuleInfo -> m (Either ProjectError Module) 
    removeModule :: ModuleInfo -> m (Either ProjectError ())
    addDeclaration :: ModuleInfo -> WithBody Declaration -> m (Either ProjectError ())
    addImport :: ModuleInfo -> WithBody Import -> m (Either ProjectError ImportId)
    removeImport :: ModuleInfo -> ImportId -> m (Either ProjectError ())
    addExport :: ModuleInfo -> WithBody Export -> m (Either ProjectError ExportId)
    removeExport :: ModuleInfo -> ExportId -> m (Either ProjectError ())
    exportAll :: ModuleInfo -> m (Either ProjectError ())
