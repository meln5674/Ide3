{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Project.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Env


instance ParamEnvClass Project ModuleInfo Module (SolutionError u) where
    addChildT = addModule
    removeChildT = removeModule
    getChildT = getModule
    setChildT = setModule

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

info :: Project -> ProjectInfo
info = projectInfo

addModule :: Monad m
          => ModuleInfo
          -> Module
          -> Project
          -> SolutionResult m u Project
addModule mi m p = case Map.lookup mi $ projectModules p of
    Just _ -> throwE $ DuplicateModule (info p) mi "Project.addModule" 
    Nothing -> return $ p{ projectModules = Map.insert mi m $ projectModules p }

removeModule :: Monad m
             => ModuleInfo
             -> Project
             -> SolutionResult m u (Module,Project)
removeModule mi p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.removeModule"
    Just m -> return (m, p{ projectModules = Map.delete mi $ projectModules p })

getModule :: Monad m
          => ModuleInfo
          -> Project
          -> SolutionResult m u Module
getModule mi p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.getModule"
    Just m -> return m

setModule :: Monad m
          => ModuleInfo
          -> ModuleInfo
          -> Module
          -> Project
          -> SolutionResult m u Project
setModule mi mi' m' p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.setModule"
    Just _ -> return $ p
        { projectModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectModules p 
        }

addExternModule :: Monad m
                => ModuleInfo
                -> ExternModule
                -> Project
                -> SolutionResult m u Project
addExternModule mi m p = case Map.lookup mi $ projectExternModules p of
    Just _ -> throwE $ DuplicateModule (info p) mi "Project.addExternModule" 
    Nothing -> return $ p{ projectExternModules = Map.insert mi m $ projectExternModules p }

removeExternModule :: Monad m
                   => ModuleInfo
                   -> Project
                   -> SolutionResult m u (ExternModule,Project)
removeExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.removeExternModule"
    Just m -> return (m, p{ projectExternModules = Map.delete mi $ projectExternModules p })

getExternModule :: Monad m
                => ModuleInfo
                -> Project
                -> SolutionResult m u ExternModule
getExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.getExternModule"
    Just m -> return m

setExternModule :: Monad m
                => ModuleInfo
                -> ModuleInfo
                -> ExternModule
                -> Project
                -> SolutionResult m u Project
setExternModule mi mi' m' p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE $ ModuleNotFound (info p) mi "Project.setExternModule"
    Just _ -> return $ p
        { projectExternModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectExternModules p 
        }

