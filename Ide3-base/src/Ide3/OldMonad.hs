{-|
Module      : Ide3.Monad
Description : Abstract interface to a Project
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides the SolutionM class, which is an abstract interface to
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

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Types
{-
-- | Abstract interface to a project
class Monad m => SolutionM m where
    -- | Load a project.
    --  This function takes no arguments, and this is by design. Instances will
    --  need to be able to encode information on how to load in their own type.
    --  While this means that types will need to either contain extra data or
    --  constructors, this means that new methods of loading can be added without
    --  breaking the interface, and does not require every instance to support
    --  all methods of loading.
    load :: SolutionResult m u ()
    -- | Create a new project
    --  See 'load' for discussion of lack of additional parameters
    new :: SolutionInfo 
        -> SolutionResult m u ()
    -- | Perform what ever actions are necessary to be able to load the current
    --  project at another point in time, e.g. saving to disc, writing to a
    --  network socket, etc.
    --  Instances are expected to perform a noop if this would do nothing
    --  See 'load' for discussion of lack of additional parameters
    finalize :: SolutionResult m u ()

    -- | Edit the solution's info
    editSolutionInfo :: (SolutionInfo -> SolutionInfo) 
                     -> SolutionResult m u ()

    -- | Add a new project to the solution
    addProject :: ProjectInfo 
               -> SolutionResult m u ()
    -- | Remove a project from the solution
    removeProject :: ProjectInfo 
                  -> SolutionResult m u ()
    -- | Get a list of the project infos in the solution
    getProjects :: SolutionResult m u [ProjectInfo]
    -- | Apply a transformation to a project's identifying information
    editProjectInfo :: ProjectInfo 
                    -> (ProjectInfo -> ProjectInfo) 
                    -> SolutionResult m u ()

    
    -- | Add a module
    addModule :: ProjectInfo 
              -> Module 
              -> SolutionResult m u ()
    -- | Add an external module
    addExternModule :: ProjectInfo 
                    -> ExternModule 
                    -> SolutionResult m u ()
    -- | Create a new module
    createModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u ()
    -- | Retrieve a module
    getModule :: ProjectInfo 
              -> ModuleInfo 
              -> SolutionResult m u Module
    -- | Retrieve an external module
    getExternModule :: ProjectInfo 
                    -> ModuleInfo 
                    -> SolutionResult m u ExternModule
    -- | Get a list of all the availible modules
    getModules :: ProjectInfo 
               -> SolutionResult m u [ModuleInfo]
    -- | Apply a transformation to a module
    editModule :: ProjectInfo -> ModuleInfo
               -> (Module -> Either (SolutionError u) Module) 
               -> SolutionResult m u ()
    -- | Remove a module
    --  Instances are expected to return a Left value if a matching module is
    --      not found
    removeModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u ()
    


    -- | Add a declaration to a module
    addDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> WithBody Declaration 
                   -> SolutionResult m u ()
    -- | Get a declaration in a module from its info
    getDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> DeclarationInfo 
                   -> SolutionResult m u (WithBody Declaration)
    -- | Get all info on all declarations in a module
    getDeclarations :: ProjectInfo 
                    -> ModuleInfo 
                    -> SolutionResult m u [DeclarationInfo]
    -- | Apply a transformation to a declaration in a module
    editDeclaration :: ProjectInfo 
                    -> ModuleInfo 
                    -> DeclarationInfo
                    -> (Declaration -> Either (SolutionError u) (WithBody Declaration))
                    -> SolutionResult m u DeclarationInfo
    -- | Remove a declaration from a module
    removeDeclaration :: ProjectInfo 
                      -> ModuleInfo 
                      -> DeclarationInfo 
                      -> SolutionResult m u ()

    -- | Add an import to a module
    addImport :: ProjectInfo 
              -> ModuleInfo 
              -> WithBody Import 
              -> SolutionResult m u ImportId
    -- | Get an import from a module
    getImport :: ProjectInfo 
              -> ModuleInfo 
              -> ImportId 
              -> SolutionResult m u (WithBody Import)
    -- | Remove an import from a module
    --  Instances are expected to return a Left value if a matching import is
    --  not found
    removeImport :: ProjectInfo 
                 -> ModuleInfo 
                 -> ImportId 
                 -> SolutionResult m u ()
    -- | Get a list of all of the import ids in a module
    getImports :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult m u [ImportId]


    -- | Add an export to a module
    addExport :: ProjectInfo 
              -> ModuleInfo 
              -> WithBody Export 
              -> SolutionResult m u ExportId
    -- | Get an export from a module
    getExport :: ProjectInfo 
              -> ModuleInfo 
              -> ExportId 
              -> SolutionResult m u (WithBody Export)
    -- | Remove an export from a module
    --  Instances are expected to return a Left value if a matching export is not found
    removeExport :: ProjectInfo 
                 -> ModuleInfo
                 -> ExportId
                 -> SolutionResult m u ()
    -- | Set a module to export all of its symbols
    exportAll :: ProjectInfo 
              -> ModuleInfo 
              -> SolutionResult m u ()
    -- | Set a module to export nothing
    exportNothing :: ProjectInfo 
                  -> ModuleInfo 
                  -> SolutionResult m u  ()
    -- | Get a list of all of the export ids in a module
    getExports :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult m u (Maybe [ExportId])
    
    -- | Add a pragma to a module
    addPragma :: ProjectInfo 
              -> ModuleInfo 
              -> Pragma 
              -> SolutionResult m u ()
    
    -- | Remove a pragma from a module
    removePragma :: ProjectInfo 
                 -> ModuleInfo 
                 -> Pragma 
                 -> SolutionResult m u ()
    
    -- | Get the pragmas in a module
    getPragmas :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult m u [Pragma]

-- | Utility function which inserts an additional transformer into a stack
-- which is topped by ExceptT
bounce :: (Monad m, MonadTrans t) => ExceptT e m a -> ExceptT e (t m) a
bounce = ExceptT . lift . runExceptT

-- | 
instance (SolutionM m) => SolutionM (StateT s m) where
    load = bounce load
    new = bounce . new
    finalize = bounce finalize
    editSolutionInfo = bounce . editSolutionInfo
    addProject = bounce . addProject
    removeProject = bounce . removeProject
    getProjects = bounce getProjects
    editProjectInfo x = bounce . editProjectInfo x
    createModule x = bounce . createModule x
    getModule x = bounce . getModule x
    getExternModule x = bounce . getExternModule x
    editDeclaration x y z = bounce . editDeclaration x y z
    addModule x = bounce . addModule x
    addExternModule x = bounce . addExternModule x
    getModules = bounce . getModules
    editModule x y = bounce . editModule x y
    removeModule x = bounce . removeModule x
    addDeclaration x y = bounce . addDeclaration x y
    getDeclaration x y = bounce . getDeclaration x y
    getDeclarations x = bounce . getDeclarations x
    removeDeclaration x y = bounce . removeDeclaration x y
    addImport x y = bounce . addImport x y
    getImport x y = bounce . getImport x y
    removeImport x y = bounce . removeImport x y
    getImports x = bounce . getImports x
    addExport x y = bounce . addExport x y
    getExport x y = bounce . getExport x y
    removeExport x y = bounce . removeExport x y
    exportAll x = bounce . exportAll x
    exportNothing x = bounce . exportNothing x
    getExports x = bounce . getExports x
    addPragma x y = bounce . addPragma x y
    removePragma x y = bounce . removePragma x y
    getPragmas x = bounce . getPragmas x
-}
