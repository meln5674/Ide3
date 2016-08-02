module Ide3.NewMonad.Instances.State.ModuleImportClass where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

instance StatefulSolutionClass m => ModuleImportClass (StatefulWrapper m) where
    addImport a b c = modifySolutionER $ \s -> runDescent4 Solution.addImport s a b c
    getImport a b c = modifySolutionER $ \s -> runDescent4 Solution.getImport s a b c
    removeImport a b c = modifySolutionER $ \s -> runDescent4 Solution.removeImport s a b c
    getImports a b = modifySolutionER $ \s -> runDescent3 Solution.getImports s a b
