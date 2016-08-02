module Ide3.NewMonad.Instances.State.ModuleExportClass where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

instance StatefulSolutionClass m => ModuleExportClass (StatefulWrapper m) where
    addExport a b c = modifySolutionER $ \s -> runDescent4 Solution.addExport s a b c
    getExport a b c = modifySolutionER $ \s -> runDescent4 Solution.getExport s a b c
    removeExport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeExport s a b c
    exportAll a b = modifySolutionER $ \s -> runDescent3 Solution.exportAll s a b
    exportNothing a b = modifySolutionER $ \s -> runDescent3 Solution.exportNothing s a b
    getExports a b = modifySolutionER $ \s -> runDescent3 Solution.getExports s a b
