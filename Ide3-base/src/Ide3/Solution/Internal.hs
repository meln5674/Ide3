{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Solution.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Env

addProject :: Monad m
           => ProjectInfo
           -> Project
           -> Solution
           -> SolutionResult m u Solution
addProject pji p s = case Map.lookup pji $ solutionProjects s of
    Just _ -> throwE $ DuplicateProject pji "Solution.addProject"
    Nothing -> return $ s{ solutionProjects = Map.insert pji p $ solutionProjects s }

removeProject :: Monad m
              => ProjectInfo
              -> Solution
              -> SolutionResult m u (Project,Solution)
removeProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.removeProject"
    Just p -> return (p, s{ solutionProjects = Map.delete pji $ solutionProjects s })

getProject :: Monad m
           => ProjectInfo
           -> Solution
           -> SolutionResult m u Project
getProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.getProject"
    Just p -> return p

setProject :: Monad m
           => ProjectInfo
           -> ProjectInfo
           -> Project
           -> Solution
           -> SolutionResult m u Solution
setProject pji pji' p' s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji "Solution.setProject"
    Just _ -> return $ s 
        { solutionProjects
            = Map.insert pji' p' 
            $ Map.delete pji 
            $ solutionProjects s 
        }



instance ParamEnvClass Solution ProjectInfo Project (SolutionError u) where
    addChildT = addProject
    removeChildT = removeProject
    getChildT = getProject
    setChildT = setProject
