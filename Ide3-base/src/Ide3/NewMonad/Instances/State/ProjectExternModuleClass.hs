{-|
Module      : Ide3.NewMonad.Instances.State.ProjectExternModuleClass
Description : Stateful implementation of the ProjectExternModuleClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ProjectExternModuleClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | Access external modules statefully
instance StatefulSolutionClass m 
      => ProjectExternModuleClass (StatefulWrapper m) where
    createExternModule = 
        modifySolutionER .-.. runDescent3 Solution.createExternModule
    getExternModules = 
        modifySolutionER .-. runDescent2 Solution.getExternModules
    removeExternModule = 
        modifySolutionER .-.. runDescent3 Solution.removeExternModule
