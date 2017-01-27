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
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad where

import Data.Text (Text)

import Ide3.Types

-- | Class of monads which can create, save, and load haskell solutions
class Monad m => PersistenceClass m where
    -- | Polymorphic token representing instructions on how to save/load, etc
    type PersistToken m
    -- | Load a solution
    load :: PersistToken m -> SolutionResult u m ()
    -- | Create a new solution
    new :: SolutionInfo -> SolutionResult u m ()
    -- | Perform what ever actions are necessary to be able to load the current
    --  solution at another point in time, e.g. saving to disc, writing to a
    --  network socket, etc.
    --  Instances are expected to perform a noop if this would do nothing
    -- If nothing is passed, instances are expected to use the most recently
    -- used token
    finalize :: Maybe (PersistToken m) -> SolutionResult u m ()

-- | Class of monads which can add, remove, edit, and retrieve projects.
class Monad m => SolutionClass m where
    -- | Edit the solution's info
    editSolutionInfo :: (SolutionInfo -> SolutionInfo) 
                     -> SolutionResult u m ()
    -- | Add a new project to the solution
    addProject :: ProjectInfo 
               -> SolutionResult u m ()
    -- | Remove a project from the solution
    removeProject :: ProjectInfo 
                  -> SolutionResult u m ()
    -- | Get a list of the project infos in the solution
    getProjects :: SolutionResult u m [ProjectInfo]
    -- | Apply a transformation to a project's identifying information
    editProjectInfo :: ProjectInfo 
                    -> (ProjectInfo -> ProjectInfo) 
                    -> SolutionResult u m ()

-- | Class of monads which can add, remove, edit, and retrieve modules
class Monad m => ProjectModuleClass m where
    -- | Create a new module
    createModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult u m ()
    -- | Get a list of all the availible modules
    getModules :: ProjectInfo 
               -> SolutionResult u m [ModuleInfo]
    -- | Remove a module
    --  Instances are expected to return a Left value if a matching module is
    --      not found
    removeModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> SolutionResult u m ()
    -- | Get the header of a module
    getModuleHeader :: ProjectInfo
                    -> ModuleInfo
                    -> SolutionResult u m Text
    -- | Edit the header of a module
    editModuleHeader :: ProjectInfo
                     -> ModuleInfo
                     -> (Text -> Text)
                     -> SolutionResult u m ()
    -- | Set a module as unparsable, and provide the contents, as well as the location
    -- and message of the error
    setModuleUnparsable :: ProjectInfo
                        -> ModuleInfo
                        -> Text
                        -> SrcLoc
                        -> String
                        -> SolutionResult u m ()
    -- | Retreive the contents of a module if it was unparsable, along with the
    -- location and message of the error, otherwise return nothing
    getUnparsableModule :: ProjectInfo
                        -> ModuleInfo
                        -> SolutionResult u m (Maybe (Text, SrcLoc, String))
    -- | Set a module as parsable and empty, the module must already exist.
    setModuleParsable :: ProjectInfo
                      -> ModuleInfo
                      -> SolutionResult u m ()
    refreshModule :: ProjectInfo
                  -> ModuleInfo
                  -> SolutionResult u m ModuleInfo

-- | Class of monads which can create, remove, and retrieve external modules
class Monad m => ProjectExternModuleClass m where
    -- | Add an external module
    createExternModule :: ProjectInfo 
                       -> ModuleInfo
                       -> SolutionResult u m ()
    getExternModules :: ProjectInfo
                     -> SolutionResult u m [ModuleInfo]
    removeExternModule :: ProjectInfo
                       -> ModuleInfo
                       -> SolutionResult u m ()

-- | Class of monads which can add, edit, remove, and retrieve declarations
class Monad m => ModuleDeclarationClass m where
    -- | Add a declaration to a module
    addDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> WithBody Declaration 
                   -> SolutionResult u m ()
    -- | Get a declaration in a module from its info
    getDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> DeclarationInfo 
                   -> SolutionResult u m (WithBody Declaration)
    -- | Get all info on all declarations in a module
    getDeclarations :: ProjectInfo 
                    -> ModuleInfo 
                    -> SolutionResult u m [DeclarationInfo]
    -- | Apply a transformation to a declaration in a module
    editDeclaration :: ProjectInfo 
                    -> ModuleInfo 
                    -> DeclarationInfo
                    -> (  WithBody Declaration 
                       -> Either (SolutionError u) (WithBody Declaration)
                       )
                    -> SolutionResult u m DeclarationInfo
    -- | Remove a declaration from a module
    removeDeclaration :: ProjectInfo 
                      -> ModuleInfo 
                      -> DeclarationInfo 
                      -> SolutionResult u m ()

-- | Class of monads which can add, remove, and retrieve imports
class Monad m => ModuleImportClass m where
    -- | Add an import to a module
    addImport :: ProjectInfo
              -> ModuleInfo 
              -> WithBody Import 
              -> SolutionResult u m ImportId
    -- | Get an import from a module
    getImport :: ProjectInfo 
              -> ModuleInfo 
              -> ImportId 
              -> SolutionResult u m (WithBody Import)
    -- | Remove an import from a module
    --  Instances are expected to return a Left value if a matching import is
    --  not found
    removeImport :: ProjectInfo 
                 -> ModuleInfo 
                 -> ImportId 
                 -> SolutionResult u m ()
    -- | Get a list of all of the import ids in a module
    getImports :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult u m [ImportId]

-- | Class of monads which can add, remove, and retrieve exports, or mark a
-- module as exporting everything
class Monad m => ModuleExportClass m where
    -- | Add an export to a module
    addExport :: ProjectInfo 
              -> ModuleInfo 
              -> WithBody Export 
              -> SolutionResult u m ExportId
    -- | Get an export from a module
    getExport :: ProjectInfo 
              -> ModuleInfo 
              -> ExportId 
              -> SolutionResult u m (WithBody Export)
    -- | Remove an export from a module
    -- Instances are expected to return a Left value if a matching export is not
    -- found
    removeExport :: ProjectInfo 
                 -> ModuleInfo
                 -> ExportId
                 -> SolutionResult u m ()
    -- | Set a module to export all of its symbols
    exportAll :: ProjectInfo 
              -> ModuleInfo 
              -> SolutionResult u m ()
    -- | Set a module to export nothing
    exportNothing :: ProjectInfo 
                  -> ModuleInfo 
                  -> SolutionResult u m  ()
    -- | Get a list of all of the export ids in a module
    getExports :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult u m (Maybe [ExportId])

-- | Class of monads which can add, remove, and retrieve pragmas
class Monad m => ModulePragmaClass m where
    -- | Add a pragma to a module
    addPragma :: ProjectInfo 
              -> ModuleInfo 
              -> Pragma 
              -> SolutionResult u m ()
    
    -- | Remove a pragma from a module
    removePragma :: ProjectInfo 
                 -> ModuleInfo 
                 -> Pragma 
                 -> SolutionResult u m ()
    
    -- | Get the pragmas in a module
    getPragmas :: ProjectInfo 
               -> ModuleInfo 
               -> SolutionResult u m [Pragma]

-- | Class of monads which can add, remove, and retrieve external exports
class Monad m => ExternModuleExportClass m where
    addExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExternExport
                    -> SolutionResult u m ExportId
    getExternExports :: ProjectInfo
                     -> ModuleInfo
                     -> SolutionResult u m [ExportId]
    getExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExportId
                    -> SolutionResult u m ExternExport
    removeExternExport :: ProjectInfo
                       -> ModuleInfo
                       -> ExportId
                       -> SolutionResult u m ()

-- | Class of monads which can convert modules to a string
class Monad m => ModuleFileClass m where
    toFile :: ProjectInfo -> ModuleInfo -> SolutionResult u m Text

-- | Class of monads which can retrieve items at locations within a module 
class ( Monad m )
     => ModuleLocationClass m where
    getModuleItemAtLocation :: ProjectInfo 
                            -> ModuleInfo
                            -> [SrcLoc]
                            -> SolutionResult u m [Maybe ( ModuleItemString
                                                         , SrcLoc
                                                         )
                                                  ]

-- | Wrapper for monads which have all project features
type ProjectClass m = (ProjectModuleClass m, ProjectExternModuleClass m)

-- | Wrapper for monads which have all module features 
type ModuleClass m = ( ModuleDeclarationClass m
                     , ModuleImportClass m
                     , ModuleExportClass m
                     , ModulePragmaClass m
                     )

-- | Wrapper for monads that have all external module features
type ExternModuleClass m = (ExternModuleExportClass m)

-- | Wrapper for monads which have all features for solutions, projects,
-- modules, and external modules
type SolutionMonad m = ( SolutionClass m
                       , ProjectClass m
                       , ModuleClass m
                       , ExternModuleClass m
                       )

-- | Wrapper for monads which have the features of SolutionMonad, but are also
-- persistent
type PersistentSolutionMonad m = (PersistenceClass m, SolutionMonad m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules
type SolutionFileMonad m = (SolutionMonad m, ModuleFileClass m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules and is persistent
type PersistentSolutionFileMonad m = ( PersistentSolutionMonad m
                                     , ModuleFileClass m
                                     )

