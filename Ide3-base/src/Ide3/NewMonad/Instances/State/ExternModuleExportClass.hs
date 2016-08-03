module Ide3.NewMonad.Instances.State.ExternModuleExportClass where

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

instance StatefulSolutionClass m => ExternModuleExportClass (StatefulWrapper m) where
    addExternExport a b c = modifySolutionER $ \s -> runDescent4 Solution.addExternExport s a b c
    getExternExport a b c = modifySolutionER $ \s -> runDescent4 Solution.getExternExport s a b c
    removeExternExport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeExternExport s a b c
    getExternExports a b = modifySolutionER $ \s -> runDescent3 Solution.getExternExports s a b

