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

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | 
instance StatefulSolutionClass m => ProjectModuleClass (StatefulWrapper m) where
    createModule a b = modifySolutionER $ \s -> runDescent3 Solution.createModule s a b
    getModules a = modifySolutionER $ \s -> runDescent2 Solution.allModules s a
    removeModule a b = modifySolutionER $ \s -> runDescent3 Solution.removeModule s a b
    getModuleHeader a b = modifySolutionER $ \s -> runDescent3 Solution.getModuleHeader s a b
    editModuleHeader a b c = modifySolutionER $ \s -> runDescent4 Solution.editModuleHeader s a b c
