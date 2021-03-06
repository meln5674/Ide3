{-|
Module      : Ide3.Solution
Description : Operations on the solution data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Env.Solution where

import Data.Text (Text)

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Ide3.Env

import Ide3.Types.Internal
import Ide3.Types.State
import Ide3.SrcLoc.Types

import Ide3.Project as Project
import qualified Ide3.Env.Project as Project

import Ide3.Solution.Internal()

-- | Add a project
addProject :: Monad m => DescentChain2 Solution ProjectInfo m u ()
addProject = do
    pji <- lift ask
    s <- get
    put =<< lift (lift $ addChildT pji (Project.new pji) s)

-- | Remove a project
removeProject :: Monad m => DescentChain2 Solution ProjectInfo m u ()
removeProject = do
    pji <- lift ask
    s <- get
    (p, s') <- lift $ lift $ removeChildT pji s
    let _ = p :: Project
    put s'

-- | Get the ids of all projects
getProjects :: Monad m => DescentChain1 Solution m u [ProjectInfo]
getProjects = gets $ Map.keys . solutionProjects

-- | Get a project by id
getProject :: Monad m => DescentChain2 Solution ProjectInfo m u Project
getProject = descend0 get

-- | Edit the info for a project
editProjectInfo :: Monad m 
                => DescentChain3 Solution 
                                 ProjectInfo 
                                 (ProjectInfo -> ProjectInfo) 
                                 m u ()
editProjectInfo = descend1 Project.editProjectInfo

-- | Get the ids of all modules in a project
allModules :: Monad m => DescentChain2 Solution ProjectInfo m u [ModuleInfo]
allModules = descend0 Project.allModules

-- | Add a local module to a project
addModule :: Monad m => DescentChain3 Solution ProjectInfo Module m u ()
addModule = descend1 Project.addModule

-- | Add an empty local module to a project
createModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
createModule = descend1 Project.createModule

-- | Remove a module from a project
removeModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
removeModule = descend1 Project.removeModule

-- | Get a local module by id from a project
getModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo m u Module
getModule = descend1 Project.getModule

-- | Apply a transformation to a module in a project
editModule :: Monad m 
           => DescentChain4 Solution 
                            ProjectInfo 
                            ModuleInfo 
                            (  Module 
                            -> Either (SolutionError u) Module
                            ) 
                            m u ()
editModule = descend2 Project.editModule

-- | Get the header from a module
getModuleHeader :: Monad m
                => DescentChain3 Solution ProjectInfo ModuleInfo m u Text
getModuleHeader = descend1 Project.getModuleHeader
    
-- | Edit the header of a module
editModuleHeader :: Monad m
                 => DescentChain4 Solution 
                                  ProjectInfo 
                                  ModuleInfo (Text -> Text) 
                                  m u ()
editModuleHeader = descend2 Project.editModuleHeader

setModuleUnparsable :: Monad m
                     => DescentChain4 Solution
                                      ProjectInfo
                                      ModuleInfo
                                      (Text, SrcLoc, String)
                        m u ()
setModuleUnparsable = descend2 Project.setModuleUnparsable

setModuleParsable :: Monad m
                   => DescentChain3 Solution
                                    ProjectInfo
                                    ModuleInfo
                      m u ()
setModuleParsable = descend1 Project.setModuleParsable

getUnparsableModule :: Monad m
                     => DescentChain3 Solution
                                      ProjectInfo
                                      ModuleInfo
                        m u (Maybe (Text, SrcLoc, String))
getUnparsableModule = descend1 Project.getUnparsableModule

-- | Add an external module to a project
addExternModule :: Monad m 
                => DescentChain3 Solution ProjectInfo ExternModule m u ()
addExternModule = descend1 Project.addExternModule

-- | Add an empty local module to a project
createExternModule :: Monad m 
                   => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
createExternModule = descend1 Project.createExternModule

-- | Get an external module by id from a project
getExternModule :: Monad m 
                => DescentChain3 Solution 
                                 ProjectInfo 
                                 ModuleInfo 
                                 m u ExternModule
getExternModule = descend1 Project.getExternModule

-- | Get the ids of all external modules in a project
getExternModules :: Monad m 
                 => DescentChain2 Solution ProjectInfo m u [ModuleInfo]
getExternModules = descend0 Project.getExternModules

-- | Remove an external module from a project
removeExternModule :: Monad m 
                   => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
removeExternModule = descend1 Project.removeExternModule

-- | Add a declaration to a module in a project
addDeclaration :: Monad m 
               => DescentChain4 Solution 
                                ProjectInfo 
                                ModuleInfo 
                                (WithBody Declaration)
                                m u ()
addDeclaration = descend2 Project.addDeclaration

-- | Remove a declaration by id in a module in a project
removeDeclaration :: Monad m 
                  => DescentChain4 Solution 
                                   ProjectInfo 
                                   ModuleInfo 
                                   DeclarationInfo 
                                   m u ()
removeDeclaration = descend2 Project.removeDeclaration

-- | Apply a transformation to a declaration in a module in a project
editDeclaration :: Monad m 
                => DescentChain5 Solution 
                                 ProjectInfo 
                                 ModuleInfo 
                                 DeclarationInfo 
                                 (  WithBody Declaration 
                                 -> Either (SolutionError u) 
                                           (WithBody Declaration)
                                 )
                                 m u DeclarationInfo
editDeclaration = descend3 Project.editDeclaration

-- | Get a declaration by id in a module in a project
getDeclaration :: Monad m 
               => DescentChain4 Solution 
                                ProjectInfo 
                                ModuleInfo 
                                DeclarationInfo 
                                m u (WithBody Declaration)
getDeclaration = descend2 Project.getDeclaration

-- | Get the ids of declarations in a module in a project
getDeclarations :: Monad m 
                => DescentChain3 Solution 
                                 ProjectInfo 
                                 ModuleInfo
                                 m u [DeclarationInfo]
getDeclarations = descend1 Project.getDeclarations

-- | Add an import to a module in a project and return the id assigned to it
addImport :: Monad m 
          => DescentChain4 Solution 
                           ProjectInfo 
                           ModuleInfo 
                           (WithBody Import) 
                           m u ImportId
addImport = descend2 Project.addImport

-- | Remove an import by id from a module in a project
removeImport :: Monad m 
             => DescentChain4 Solution ProjectInfo ModuleInfo ImportId m u ()
removeImport = descend2 Project.removeImport

-- | Get an import by id from a module in a project
getImport :: Monad m 
          => DescentChain4 Solution 
                           ProjectInfo 
                           ModuleInfo 
                           ImportId 
                           m u (WithBody Import)
getImport = descend2 Project.getImport

-- | Get the ids of all imports in a module in a project
getImports :: Monad m 
           => DescentChain3 Solution ProjectInfo ModuleInfo m u [ImportId]
getImports = descend1 Project.getImports

-- | Set a module in a project to export all of its symbols
exportAll :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
exportAll = descend1 Project.exportAll

-- | Add an export to a module in a project and return the id assigned to it
addExport :: Monad m 
          => DescentChain4 Solution 
                           ProjectInfo 
                           ModuleInfo 
                           (WithBody Export) 
                           m u ExportId
addExport = descend2 Project.addExport

-- | Remove an export by id from a module in a project
removeExport :: Monad m 
             => DescentChain4 Solution ProjectInfo ModuleInfo ExportId m u ()
removeExport = descend2 Project.removeExport

-- | Set a module in a project to export none of its symbols
exportNothing :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo m u ()
exportNothing = descend1 Project.exportNothing

-- | Get an export by id from a module in a project
getExport :: Monad m 
          => DescentChain4 Solution 
                           ProjectInfo 
                           ModuleInfo 
                           ExportId 
                           m u (WithBody Export)
getExport = descend2 Project.getExport

-- | Get the ids of all exports in a module in a project, or signal that the
-- module is set to export everything
getExports :: Monad m 
           => DescentChain3 Solution 
                            ProjectInfo 
                            ModuleInfo 
                            m u (Maybe [ExportId])
getExports = descend1 Project.getExports

-- | Add a pragma to a module in a project
addPragma :: Monad m 
          => DescentChain4 Solution ProjectInfo ModuleInfo Pragma m u ()
addPragma = descend2 Project.addPragma

-- | Remove a pragma from a module in a project
removePragma :: Monad m 
             => DescentChain4 Solution ProjectInfo ModuleInfo Pragma m u ()
removePragma = descend2 Project.removePragma

-- | Get the pragmas in a module in a project
getPragmas :: Monad m 
           => DescentChain3 Solution ProjectInfo ModuleInfo m u [Pragma]
getPragmas = descend1 Project.getPragmas

-- | Add an export and return the id assigned to it
addExternExport :: Monad m 
                => DescentChain4 Solution 
                                 ProjectInfo 
                                 ModuleInfo 
                                 ExternExport 
                                 m u ExportId
addExternExport = descend2 Project.addExternExport

-- | Remove an export by id
removeExternExport :: Monad m 
                   => DescentChain4 Solution 
                                    ProjectInfo 
                                    ModuleInfo 
                                    ExportId 
                                    m u ()
removeExternExport = descend2 Project.removeExternExport

-- | Get an export by id
getExternExport :: Monad m 
                => DescentChain4 Solution 
                                 ProjectInfo 
                                 ModuleInfo 
                                 ExportId 
                                 m u ExternExport
getExternExport = descend2 Project.getExternExport

-- | Get the ids of all exports, or signify that all symbols are exported
getExternExports :: Monad m 
                 => DescentChain3 Solution 
                                  ProjectInfo 
                                  ModuleInfo 
                                  m u [ExportId]
getExternExports = descend1 Project.getExternExports
