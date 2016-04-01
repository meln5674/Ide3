module Ide3.Monad where

import Ide3.Types

class Monad m => ProjectM m where
    load :: m ()
    new :: ProjectInfo -> m ()
    finalize :: m ()
    editProjectInfo :: (ProjectInfo -> ProjectInfo) -> m ()
    addModule :: Module -> m ()
    createModule :: ModuleInfo -> m ()
    getModule :: ModuleInfo -> m (Either ProjectError Module) 
    addDeclaration :: ModuleInfo -> WithBody Declaration -> m ()
    addImport :: ModuleInfo -> WithBody Import -> m ()
    removeImport :: ModuleInfo -> WithBody Import -> m ()
    addExport :: ModuleInfo -> WithBody Export -> m ()
    removeExport :: ModuleInfo -> WithBody Export -> m ()
    exportAll :: ModuleInfo -> m ()
