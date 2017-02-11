{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Ide3.NewerMonad where

import Control.Monad.Except

import Ide3.Types

class (MonadError (SolutionError u) m) => PersistenceClass u m where
    -- | Load a project.
    load :: m ()
    -- | Create a new project
    new :: SolutionInfo -> m ()
    -- | Perform what ever actions are necessary to be able to load the current
    --  project at another point in time, e.g. saving to disc, writing to a
    --  network socket, etc.
    --  Instances are expected to perform a noop if this would do nothing
    finalize :: m ()

-- | Class of monads which can add, remove, edit, and retrieve projects.
class (MonadError (SolutionError u) m) => SolutionClass u m where
    -- | Edit the solution's info
    editSolutionInfo :: (SolutionInfo -> SolutionInfo) 
                     -> m ()
    -- | Add a new project to the solution
    addProject :: ProjectInfo 
               -> m ()
    -- | Remove a project from the solution
    removeProject :: ProjectInfo 
                  -> m ()
    -- | Get a list of the project infos in the solution
    getProjects :: m [ProjectInfo]
    -- | Apply a transformation to a project's identifying information
    editProjectInfo :: ProjectInfo 
                    -> (ProjectInfo -> ProjectInfo) 
                    -> m ()

-- | Class of monads which can add, remove, edit, and retrieve modules
class (MonadError (SolutionError u) m) => ProjectModuleClass u m where
    -- | Create a new module
    createModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> m ()
    -- | Get a list of all the availible modules
    getModules :: ProjectInfo 
               -> m [ModuleInfo]
    -- | Remove a module
    --  Instances are expected to return a Left value if a matching module is
    --      not found
    removeModule :: ProjectInfo 
                 -> ModuleInfo 
                 -> m ()
    -- | Get the header of a module
    getModuleHeader :: ProjectInfo
                    -> ModuleInfo
                    -> m String
    -- | Edit the header of a module
    editModuleHeader :: ProjectInfo
                     -> ModuleInfo
                     -> (String -> String)
                     -> m ()
    -- | Set a module as unparsable, and provide the contents
    setModuleUnparsable :: ProjectInfo
                        -> ModuleInfo
                        -> String
                        -> m ()
    -- | Retreive the contents of a module if it was unparsable, otherwise
    -- return nothing
    getUnparsableModule :: ProjectInfo
                        -> ModuleInfo
                        -> m (Maybe String)
    -- | Set a module as parsable and empty, the module must already exist.
    setModuleParsable :: ProjectInfo
                      -> ModuleInfo
                      -> m ()
    refreshModule :: ProjectInfo
                  -> ModuleInfo
                  -> m ModuleInfo

-- | Class of monads which can create, remove, and retrieve external modules
class (MonadError (SolutionError u) m) => ProjectExternModuleClass u m where
    -- | Add an external module
    createExternModule :: ProjectInfo 
                       -> ModuleInfo
                       -> m ()
    getExternModules :: ProjectInfo
                     -> m [ModuleInfo]
    removeExternModule :: ProjectInfo
                       -> ModuleInfo
                       -> m ()

-- | Class of monads which can add, edit, remove, and retrieve declarations
class (MonadError (SolutionError u) m) => ModuleDeclarationClass u m where
    -- | Add a declaration to a module
    addDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> WithBody Declaration 
                   -> m ()
    -- | Get a declaration in a module from its info
    getDeclaration :: ProjectInfo 
                   -> ModuleInfo 
                   -> DeclarationInfo 
                   -> m (WithBody Declaration)
    -- | Get all info on all declarations in a module
    getDeclarations :: ProjectInfo 
                    -> ModuleInfo 
                    -> m [DeclarationInfo]
    -- | Apply a transformation to a declaration in a module
    editDeclaration :: ProjectInfo 
                    -> ModuleInfo 
                    -> DeclarationInfo
                    -> (  WithBody Declaration 
                       -> Either (SolutionError u) (WithBody Declaration)
                       )
                    -> m DeclarationInfo
    -- | Remove a declaration from a module
    removeDeclaration :: ProjectInfo 
                      -> ModuleInfo 
                      -> DeclarationInfo 
                      -> m ()

-- | Class of monads which can add, remove, and retrieve imports
class (MonadError (SolutionError u) m) => ModuleImportClass u m where
    -- | Add an import to a module
    addImport :: ProjectInfo
              -> ModuleInfo 
              -> WithBody Import 
              -> m ImportId
    -- | Get an import from a module
    getImport :: ProjectInfo 
              -> ModuleInfo 
              -> ImportId 
              -> m (WithBody Import)
    -- | Remove an import from a module
    --  Instances are expected to return a Left value if a matching import is
    --  not found
    removeImport :: ProjectInfo 
                 -> ModuleInfo 
                 -> ImportId 
                 -> m ()
    -- | Get a list of all of the import ids in a module
    getImports :: ProjectInfo 
               -> ModuleInfo 
               -> m [ImportId]

-- | Class of monads which can add, remove, and retrieve exports, or mark a
-- module as exporting everything
class (MonadError (SolutionError u) m) => ModuleExportClass u m where
    -- | Add an export to a module
    addExport :: ProjectInfo 
              -> ModuleInfo 
              -> WithBody Export 
              -> m ExportId
    -- | Get an export from a module
    getExport :: ProjectInfo 
              -> ModuleInfo 
              -> ExportId 
              -> m (WithBody Export)
    -- | Remove an export from a module
    -- Instances are expected to return a Left value if a matching export is not
    -- found
    removeExport :: ProjectInfo 
                 -> ModuleInfo
                 -> ExportId
                 -> m ()
    -- | Set a module to export all of its symbols
    exportAll :: ProjectInfo 
              -> ModuleInfo 
              -> m ()
    -- | Set a module to export nothing
    exportNothing :: ProjectInfo 
                  -> ModuleInfo 
                  -> m  ()
    -- | Get a list of all of the export ids in a module
    getExports :: ProjectInfo 
               -> ModuleInfo 
               -> m (Maybe [ExportId])

-- | Class of monads which can add, remove, and retrieve pragmas
class (MonadError (SolutionError u) m) => ModulePragmaClass u m where
    -- | Add a pragma to a module
    addPragma :: ProjectInfo 
              -> ModuleInfo 
              -> Pragma 
              -> m ()
    
    -- | Remove a pragma from a module
    removePragma :: ProjectInfo 
                 -> ModuleInfo 
                 -> Pragma 
                 -> m ()
    
    -- | Get the pragmas in a module
    getPragmas :: ProjectInfo 
               -> ModuleInfo 
               -> m [Pragma]

-- | Class of monads which can add, remove, and retrieve external exports
class (MonadError (SolutionError u) m) => ExternModuleExportClass u m where
    addExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExternExport
                    -> m ExportId
    getExternExports :: ProjectInfo
                     -> ModuleInfo
                     -> m [ExportId]
    getExternExport :: ProjectInfo
                    -> ModuleInfo
                    -> ExportId
                    -> m ExternExport
    removeExternExport :: ProjectInfo
                       -> ModuleInfo
                       -> ExportId
                       -> m ()

-- | Class of monads which can convert modules to a string
class (MonadError (SolutionError u) m) => ModuleFileClass u m where
    toFile :: ProjectInfo -> ModuleInfo -> m String

-- | Class of monads which can retrieve items at locations within a module 
class ( (MonadError (SolutionError u) m) )
     => ModuleLocationClass u m where
    getModuleItemAtLocation :: ProjectInfo 
                            -> ModuleInfo
                            -> [SrcLoc]
                            -> m [Maybe ( ModuleItemString
                                                         , SrcLoc
                                                         )
                                                  ]

-- | Wrapper for monads which have all project features
type ProjectClass u m = (ProjectModuleClass u m, ProjectExternModuleClass u m)

-- | Wrapper for monads which have all module features 
type ModuleClass u m = ( ModuleDeclarationClass u m
                     , ModuleImportClass u m
                     , ModuleExportClass u m
                     , ModulePragmaClass u m
                     )

-- | Wrapper for monads that have all external module features
type ExternModuleClass u m = (ExternModuleExportClass u m)

-- | Wrapper for monads which have all features for solutions, projects,
-- modules, and external modules
type SolutionMonad u m = ( SolutionClass u m
                       , ProjectClass u m
                       , ModuleClass u m
                       , ExternModuleClass u m
                       )

-- | Wrapper for monads which have the features of SolutionMonad, but are also
-- persistent
type PersistentSolutionMonad u m = (PersistenceClass u m, SolutionMonad u m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules
type SolutionFileMonad u m = (SolutionMonad u m, ModuleFileClass u m)

-- | Wrapper for monads which have the features of SolutionMonad, but also can
-- produce files for modules and is persistent
type PersistentSolutionFileMonad u m = ( PersistentSolutionMonad u m
                                     , ModuleFileClass u m
                                     )
