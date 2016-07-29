{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Project.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types
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

addModule mi m p = case Map.lookup mi $ projectModules p of
    Just _ -> throwE $ DuplicateModule mi "Project.addModule" 
    Nothing -> return $ p{ projectModules = Map.insert mi m $ projectModules p }

removeModule mi p = case Map.lookup mi $ projectModules p of
    Nothing -> throwE undefined
    Just m -> return (m, p{ projectModules = Map.delete mi $ projectModules p })

getModule mi p = case Map.lookup mi $ projectModules p of
    Just m -> return m
    Nothing -> throwE undefined

setModule mi mi' m' p = case Map.lookup mi $ projectModules p of
    Just _ -> return $ p
        { projectModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectModules p 
        }
    Nothing -> throwE undefined

addExternModule mi m p = case Map.lookup mi $ projectExternModules p of
    Just _ -> throwE $ DuplicateModule mi "Project.addExternModule" 
    Nothing -> return $ p{ projectExternModules = Map.insert mi m $ projectExternModules p }

removeExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Nothing -> throwE undefined
    Just m -> return (m, p{ projectExternModules = Map.delete mi $ projectExternModules p })

getExternModule mi p = case Map.lookup mi $ projectExternModules p of
    Just m -> return m
    Nothing -> throwE undefined

setExternModule mi mi' m' p = case Map.lookup mi $ projectExternModules p of
    Just _ -> return $ p
        { projectExternModules
            = Map.insert mi' m' 
            $ Map.delete mi 
            $ projectExternModules p 
        }
    Nothing -> throwE undefined

