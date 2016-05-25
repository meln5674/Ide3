{-|
Module      : Ide3.Monad
Description : Abstract interface to a Project
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides the ProjectM class, which is an abstract interface to
a Project. Instances must provide the ability to create, save, load, and query
a project, as well as add and remove modules, exports, imports, and declarations.

This approach allows operations to be performed "within the context of a project",
without actually needing to have a project present. This is required to resolve
circular dependencies.

This also means that the backing store for an application build on this library can
be swapped without the application itself, and also means that mocking the interface
for testing is simpler and does not require any IO.

Instances are not required to use the Project type, but it is provided as a
starting point.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.Monad where

import Control.Monad.Trans.Except

import Ide3.Types

type ProjectResult m u = ExceptT (ProjectError u) m

-- |Abstract interface to a project
class Monad m => ProjectM m where
    -- |Load a project.
    --  This function takes no arguments, and this is by design. Instances will
    --  need to be able to encode information on how to load in their own type.
    --  While this means that types will need to either contain extra data or
    --  constructors, this means that new methods of loading can be added without
    --  breaking the interface, and does not require every instance to support
    --  all methods of loading.
    load :: ProjectResult m u ()
    -- |Create a new project
    --  See 'load' for discussion of lack of additional parameters
    new :: ProjectInfo -> ProjectResult m u ()
    -- |Perform what ever actions are necessary to be able to load the current
    --  project at another point in time, e.g. saving to disc, writing to a
    --  network socket, etc.
    --  Instances are expected to perform a noop if this would do nothing
    --  See 'load' for discussion of lack of additional parameters
    finalize :: ProjectResult m u ()


    -- |Apply a transformation to the projects identifying information
    editProjectInfo :: (ProjectInfo -> ProjectInfo) -> ProjectResult m u ()


    -- |Add a module
    addModule :: Module -> ProjectResult m u ()
    -- | Add an external module
    addExternModule :: ExternModule -> ProjectResult m u ()
    -- |Create a new module
    createModule :: ModuleInfo -> ProjectResult m u ()
    -- |Retrieve a module
    getModule :: ModuleInfo -> ProjectResult m u Module
    -- | Retrieve an external module
    getExternModule :: ModuleInfo -> ProjectResult m u ExternModule
    -- |Get a list of all the availible modules
    getModules :: ProjectResult m u [ModuleInfo]
    -- | Apply a transformation to a module
    editModule :: ModuleInfo -> (Module -> Either (ProjectError u) Module) -> ProjectResult m u ()
    -- |Remove a module
    --  Instances are expected to return a Left value if a matching module is
    --      not found
    removeModule :: ModuleInfo -> ProjectResult m u ()
    


    -- |Add a declaration to a module
    addDeclaration :: ModuleInfo -> WithBody Declaration -> ProjectResult m u ()
    -- | Get a declaration in a module from its info
    getDeclaration :: ModuleInfo -> DeclarationInfo -> ProjectResult m u (WithBody Declaration)
    -- | Get all info on all declarations in a module
    getDeclarations :: ModuleInfo -> ProjectResult m u [DeclarationInfo]
    -- | Apply a transformation to a declaration in a module
    editDeclaration :: ModuleInfo 
                    -> DeclarationInfo 
                    -> (Declaration -> Either (ProjectError u) (WithBody Declaration))
                    -> ProjectResult m u ()
    -- | Remove a declaration from a module
    removeDeclaration :: ModuleInfo -> DeclarationInfo -> ProjectResult m u ()

    -- |Add an import to a module
    addImport :: ModuleInfo -> WithBody Import -> ProjectResult m u ImportId
    -- | Get an import from a module
    getImport :: ModuleInfo -> ImportId -> ProjectResult m u (WithBody Import)
    -- |Remove an import from a module
    --  Instances are expected to return a Left value if a matching import is
    --  not found
    removeImport :: ModuleInfo -> ImportId -> ProjectResult m u ()
    -- | Get a list of all of the import ids in a module
    getImports :: ModuleInfo -> ProjectResult m u [ImportId]


    -- |Add an export to a module
    addExport :: ModuleInfo -> WithBody Export -> ProjectResult m u ExportId
    -- | Get an export from a module
    getExport :: ModuleInfo -> ExportId -> ProjectResult m u (WithBody Export)
    -- |Remove an export from a module
    --  Instances are expected to return a Left value if a matching export is not found
    removeExport :: ModuleInfo -> ExportId -> ProjectResult m u ()
    -- |Set a module to export all of its symbols
    exportAll :: ModuleInfo -> ProjectResult m u ()
    -- | Set a module to export nothing
    exportNothing :: ModuleInfo -> ProjectResult m u  ()
    -- | Get a list of all of the export ids in a module
    getExports :: ModuleInfo -> ProjectResult m u (Maybe [ExportId])
