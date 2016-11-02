{-|
Module      : Ide3.Solution.Internal
Description : Top level operations on the solution data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Solution.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Types.State
import Ide3.Env

-- | Add a project to a solution
addProject :: Monad m
           => ProjectInfo
           -> Project
           -> Solution
           -> SolutionResult u m Solution
addProject pji p s = case Map.lookup pji $ solutionProjects s of
    Just _ -> throwE $ DuplicateProject pji "Solution.addProject"
    Nothing -> return $ s{ solutionProjects = Map.insert pji p $ solutionProjects s }

-- | Remove a project from a solution
removeProject :: Monad m
              => ProjectInfo
              -> Solution
              -> SolutionResult u m (Project,Solution)
removeProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.removeProject"
    Just p -> return (p, s{ solutionProjects = Map.delete pji $ solutionProjects s })

-- | Get a project from a solution
getProject :: Monad m
           => ProjectInfo
           -> Solution
           -> SolutionResult u m Project
getProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.getProject"
    Just p -> return p

-- | Update a project in a solution
setProject :: Monad m
           => ProjectInfo
           -> ProjectInfo
           -> Project
           -> Solution
           -> SolutionResult u m Solution
setProject pji pji' p' s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.setProject"
    Just _ -> return $ s 
        { solutionProjects
            = Map.insert pji' p' 
            $ Map.delete pji 
            $ solutionProjects s 
        }

-- | Add, remove, retreive, and overwrite projects
instance ParamEnvClass Solution ProjectInfo Project (SolutionError u) where
    addChildT = addProject
    removeChildT = removeProject
    getChildT = getProject
    setChildT = setProject
