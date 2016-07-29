{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Solution.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Env

addProject pi p s = case Map.lookup pi $ solutionProjects s of
    Just _ -> throwE $ DuplicateProject pi $ "Solution.addProject"
    Nothing -> return $ s{ solutionProjects = Map.insert pi p $ solutionProjects s }

removeProject pi s = case Map.lookup pi $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pi $ "Solution.removeProject"
    Just p -> return (p, s{ solutionProjects = Map.delete pi $ solutionProjects s })

getProject pi s = case Map.lookup pi $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pi $ "Solution.getProject"
    Just p -> return p

setProject pi pi' p' s = case Map.lookup pi $ solutionProjects s of
    Nothing -> throwE $ ProjectNotFound pi $ "Solution.setProject"
    Just _ -> return $ s 
        { solutionProjects
            = Map.insert pi' p' 
            $ Map.delete pi 
            $ solutionProjects s 
        }



instance ParamEnvClass Solution ProjectInfo Project (SolutionError u) where
    addChildT = addProject
    removeChildT = removeProject
    getChildT = getProject
    setChildT = setProject
