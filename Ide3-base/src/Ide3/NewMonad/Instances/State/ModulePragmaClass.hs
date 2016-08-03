module Ide3.NewMonad.Instances.State.ModulePragmaClass where

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

instance StatefulSolutionClass m => ModulePragmaClass (StatefulWrapper m) where
    addPragma a b c = modifySolutionER $ \s -> runDescent4 Solution.addPragma s a b c
    removePragma a b c = modifySolutionER $ \s -> runDescent4 Solution.removePragma s a b c
    getPragmas a b = modifySolutionER $ \s -> runDescent3 Solution.getPragmas s a b