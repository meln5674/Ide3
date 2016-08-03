module Ide3.NewMonad.Instances.State.ProjectExternModuleClass where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

instance StatefulSolutionClass m => ProjectExternModuleClass (StatefulWrapper m) where
    addExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.addExternModule s a b
    getExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.getExternModule s a b
    getExternModules a = modifySolutionER $ \s -> runDescent2 Solution.getExternModules s a
    removeExternModule a b = modifySolutionER $ \s -> runDescent3 Solution.removeExternModule s a b
