{-|
Module      : Ide3.NewMonad.Instances.State.ModuleExportClass
Description : Stateful implementation of the ModuleExportClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModuleExportClass where

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

instance StatefulSolutionClass m => ModuleExportClass (StatefulWrapper m) where
    addExport a b c = modifySolutionER $ \s -> runDescent4 Solution.addExport s a b c
    getExport a b c = modifySolutionER $ \s -> runDescent4 Solution.getExport s a b c
    removeExport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeExport s a b c
    exportAll a b = modifySolutionER $ \s -> runDescent3 Solution.exportAll s a b
    exportNothing a b = modifySolutionER $ \s -> runDescent3 Solution.exportNothing s a b
    getExports a b = modifySolutionER $ \s -> runDescent3 Solution.getExports s a b
