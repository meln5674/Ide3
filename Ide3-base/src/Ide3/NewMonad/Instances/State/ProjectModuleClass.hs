{-|
Module      : Ide3.NewMonad.Instances.State.ProjectModuleClass
Description : Stateful implementation of the ProjectModuleClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ProjectModuleClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | 
instance StatefulSolutionClass m => ProjectModuleClass (StatefulWrapper m) where
    createModule = modifySolutionER .-.. runDescent3 Solution.createModule
    getModules = modifySolutionER .-. runDescent2 Solution.allModules
    removeModule = modifySolutionER .-.. runDescent3 Solution.removeModule
    getModuleHeader = modifySolutionER .-.. runDescent3 Solution.getModuleHeader
    editModuleHeader = modifySolutionER .-... runDescent4 Solution.editModuleHeader
