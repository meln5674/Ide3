{-|
Module      : Ide3.NewMonad.Instances.State.SolutionClass
Description : Stateful implementation of the SolutionClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.SolutionClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types.Internal
import Ide3.Types.State

-- | 
instance StatefulSolutionClass m => SolutionClass (StatefulWrapper m) where
    editSolutionInfo f = modifySolution $ \s -> s{ solutionInfo = f $ solutionInfo s }
    addProject = modifySolutionER .-. runDescent2 Solution.addProject
    removeProject = modifySolutionER .-. runDescent2 Solution.removeProject
    getProjects = modifySolutionER $ runDescent1 Solution.getProjects
    editProjectInfo = modifySolutionER .-.. runDescent3 Solution.editProjectInfo
