{-# LANGUAGE ConstraintKinds #-}
module Ide3.NewMonad where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

class Monad m => PersistenceClass m where
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

class Monad m => SolutionClass m where
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


class Monad m => ProjectModuleClass m where
    -- | Add a module
    addModule :: ProjectInfo 
              -> Module 
              -> SolutionResult m u ()
    
    -- | Create a new module
    createModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u ()
    -- | Retrieve a module
    getModule :: ProjectInfo 
              -> ModuleInfo 
              -> SolutionResult m u Module
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

class Monad m => ProjectExternModuleClass m where
    -- | Add an external module
    addExternModule :: ProjectInfo 
                    -> ExternModule 
                    -> SolutionResult m u ()
    -- | Retrieve an external module
    getExternModule :: ProjectInfo 
                    -> ModuleInfo 
                    -> SolutionResult m u ExternModule
    getExternModules :: ProjectInfo
                     -> SolutionResult m u [ModuleInfo]
    removeExternModule :: ProjectInfo
                       -> ModuleInfo
                       -> SolutionResult m u ()

{-
class Monad m => ProjectDependencyClass m where
    addDependency :: ProjectInfo
                  -> Dependency
                  -> SolutionResult m u ()
    removeDependency :: ProjectInfo
                     -> Dependency
                     -> SolutionResult m u ()
    getDependencies :: ProjectInfo
                    -> SolutionResult m u [Dependency]
-}

class Monad m => ModuleDeclarationClass m where
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

class Monad m => ModuleImportClass m where
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

class Monad m => ModuleExportClass m where
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


class Monad m => ModulePragmaClass m where
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



type ProjectClass m = (ProjectModuleClass m, ProjectExternModuleClass m)
type ModuleClass m = (ModuleDeclarationClass m, ModuleImportClass m, ModuleExportClass m, ModulePragmaClass m)
type SolutionMonad m = (SolutionClass m, ProjectClass m, ModuleClass m)
type PersistentSolutionMonad m = (PersistenceClass m, SolutionMonad m)
