{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Solution.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Env

addProject pji p s = case Map.lookup pji $ solutionProjects s of
    Just _ -> throwE $ DuplicateProject pji $ "Solution.addProject"
    Nothing -> return $ s{ solutionProjects = Map.insert pji p $ solutionProjects s }

removeProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji $ "Solution.removeProject"
    Just p -> return (p, s{ solutionProjects = Map.delete pji $ solutionProjects s })

getProject pji s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji $ "Solution.getProject"
    Just p -> return p

setProject pji pji' p' s = case Map.lookup pji $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pji $ "Solution.setProject"
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
