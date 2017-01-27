{-|
Module      : Ide3.Project.Internal
Description : Top level operations on the project data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide3.Project.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Types.State
import Ide3.Env

-- | Add, remove, retrieve, and overwrite modules
instance ParamEnvClass Project ModuleInfo Module (SolutionError u) where
    addChildT = addModule
    removeChildT = removeModule
    getChildT = getModule
    setChildT = setModule

-- | Add, remove retrieve, and overwrite external modules
instance ParamEnvClass Project ModuleInfo ExternModule (SolutionError u) where
    addChildT = addExternModule
    removeChildT = removeExternModule
    getChildT = getExternModule
    setChildT = setExternModule


-- | Create a new project from info
new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo Map.empty

-- |Create an empry project
empty :: Project
empty = Project (ProjectInfo "") Map.empty BuildInfo Map.empty

-- | Get the info from a project
info :: Project -> ProjectInfo
info = projectInfo

-- | Add a module to a project
addModule :: Monad m
          => ModuleInfo
          -> Module
          -> Project
          -> SolutionResult u m Project
addModule mi m p = case Map.lookup mi $ projectModules p of
    Just _ -> throwE $ DuplicateModule (info p) mi "Project.addModule" 
    Nothing -> return $ p{ projectModules = Map.insert mi m $ projectModules p }

-- | Remove a module from a project
removeModule :: Monad m
             => ModuleInfo
             -> Project
             -> SolutionResult u m (Module,Project)
removeModule mi p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.removeModule"
    Just m -> return (m, p{ projectModules = Map.delete mi $ projectModules p })

-- | Get a module from a project
getModule :: Monad m
          => ModuleInfo
          -> Project
          -> SolutionResult u m Module
getModule mi p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.getModule"
    Just m -> return m

-- | Update a module in a project
setModule :: Monad m
          => ModuleInfo
          -> ModuleInfo
          -> Module
          -> Project
          -> SolutionResult u m Project
setModule mi mi' m' p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.setModule"
    Just _ -> return $ p
        { projectModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectModules p 
        }

-- | Add an external module to a project
addExternModule :: Monad m
                => ModuleInfo
                -> ExternModule
                -> Project
                -> SolutionResult u m Project
addExternModule mi m p = case Map.lookup mi $ projectExternModules p of
    Just _ -> throwE $ DuplicateModule (info p) mi "Project.addExternModule" 
    Nothing -> return p
        { projectExternModules = Map.insert mi m $ projectExternModules p }

-- | Remove an external module from a project
removeExternModule :: Monad m
                   => ModuleInfo
                   -> Project
                   -> SolutionResult u m (ExternModule,Project)
removeExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.removeExternModule"
    Just m -> return 
        ( m
        , p{ projectExternModules = Map.delete mi $ projectExternModules p }
        )

-- | Get an external module from a project
getExternModule :: Monad m
                => ModuleInfo
                -> Project
                -> SolutionResult u m ExternModule
getExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.getExternModule"
    Just m -> return m

-- | Update an external module in a project
setExternModule :: Monad m
                => ModuleInfo
                -> ModuleInfo
                -> ExternModule
                -> Project
                -> SolutionResult u m Project
setExternModule mi mi' m' p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.setExternModule"
    Just _ -> return $ p
        { projectExternModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectExternModules p 
        }

