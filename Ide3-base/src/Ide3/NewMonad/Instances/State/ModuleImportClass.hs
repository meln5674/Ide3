{-|
Module      : Ide3.NewMonad.Instances.State.ModuleImportClass
Description : Stateful implementation of the ModuleImportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModuleImportClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | Access imports statefully
instance StatefulSolutionClass m => ModuleImportClass (StatefulWrapper m) where
    addImport = modifySolutionER .-... runDescent4 Solution.addImport
    getImport = modifySolutionER .-... runDescent4 Solution.getImport
    removeImport = modifySolutionER .-... runDescent4 Solution.removeImport
    getImports = modifySolutionER .-.. runDescent3 Solution.getImports
