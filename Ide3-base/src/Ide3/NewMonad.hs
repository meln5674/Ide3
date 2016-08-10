{-|
Module      : Ide3.NewMonad
Description : Typesclasses for creating, editing, and reading haskell projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

The classes in this module are populated by monads which can perform operations
on specific parts of a haskell project.

-}

{-# LANGUAGE ConstraintKinds #-}
module Ide3.NewMonad where

import Control.Monad.Trans.Except

import Ide3.Types

-- | Class of monads which can create, save, and load haskell projects
-- The methods in this class take no arguments such as file paths, and this is
-- intentional. Monads in this class are expected to be able to store the 
-- information for its specific type of loading.
class Monad m => PersistenceClass m where
    -- | Load a project.
    load :: SolutionResult m u ()
    -- | Create a new project
    new :: SolutionInfo 
        -> SolutionResult m u ()
    -- | Perform what ever actions are necessary to be able to load the current
    --  project at another point in time, e.g. saving to disc, writing to a
    --  network socket, etc.
    --  Instances are expected to perform a noop if this would do nothing
    finalize :: SolutionResult m u ()

-- | Class of monads which can add, remove, edit, and retreive projects.
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

-- | Class of monads which can add, remove, edit, and retreive modules
class Monad m => ProjectModuleClass m where
    -- | Create a new module
    createModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u ()
    -- | Get a list of all the availible modules
    getModules :: ProjectInfo 
               -> SolutionResult m u [ModuleInfo]
    -- | Remove a module
    --  Instances are expected to return a Left value if a matching module is
    --      not found
    removeModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult m u ()
    -- | Get the header of a module
    getModuleHeader :: ProjectInfo
                    -> ModuleInfo
                    -> SolutionResult m u String
    -- | Edit the header of a module
    editModuleHeader :: ProjectInfo
                     -> ModuleInfo
                     -> (String -> String)
                     -> SolutionResult m u ()

-- | Class of monads which can create, remove, and retrieve external modules
class Monad m => ProjectExternModuleClass m where
    -- | Add an external module
    createExternModule :: ProjectInfo 
                       -> ModuleInfo
                       -> SolutionResult m u ()
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

-- | Class of monads which can add, edit, remove, and retreive declarations
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
                    -> (WithBody Declaration -> Either (SolutionError u) (WithBody Declaration))
                    -> SolutionResult m u DeclarationInfo
    -- | Remove a declaration from a module
    removeDeclaration :: ProjectInfo 
                      -> ModuleInfo 
                      -> DeclarationInfo 
                      -> SolutionResult m u ()

-- | Class of monads which can add, remove, and retrieve imports
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

-- | Class of monads which can add, remove, and retrieve exports, or mark a
-- module as exporting everything
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

-- | Class of monads which can add, remove, and retrieve pragmas
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

-- | Class of monads which can add, remove, and retrieve external exports
class Monad m => ExternModuleExportClass m where
    addExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExternExport
                    -> SolutionResult m u ExportId
    getExternExports :: ProjectInfo
                     -> ModuleInfo
                     -> SolutionResult m u [ExportId]
    getExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExportId
                    -> SolutionResult m u ExternExport
    removeExternExport :: ProjectInfo
                       -> ModuleInfo
                       -> ExportId
                       -> SolutionResult m u ()

-- | Class of monads which can convert modules to a string
class Monad m => ModuleFileClass m where
    toFile :: ProjectInfo -> ModuleInfo -> SolutionResult m u String

-- | Wrapper for monads which have all project features
type ProjectClass m = (ProjectModuleClass m, ProjectExternModuleClass m)

-- | Wrapper for monads which have all module features 
type ModuleClass m = (ModuleDeclarationClass m, ModuleImportClass m, ModuleExportClass m, ModulePragmaClass m)

-- | Wrapper for monads that have all external module features
type ExternModuleClass m = (ExternModuleExportClass m)

-- | Wrapper for monads which have all features for solutions, projects,
-- modules, and external modules
type SolutionMonad m = (SolutionClass m, ProjectClass m, ModuleClass m, ExternModuleClass m)

-- | Wrapper for monads which have the features of SolutionMonad, but are also persistent
type PersistentSolutionMonad m = (PersistenceClass m, SolutionMonad m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules
type SolutionFileMonad m = (SolutionMonad m, ModuleFileClass m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules and is persistent
type PersistentSolutionFileMonad m = (PersistentSolutionMonad m, ModuleFileClass m)

-- | A utility class. Types in this class can insert themselves underneath
-- ExceptT in a monad transformer stack
class MonadBounce t where
    bounce :: (Monad m) => ExceptT e m a -> ExceptT e (t m) a
