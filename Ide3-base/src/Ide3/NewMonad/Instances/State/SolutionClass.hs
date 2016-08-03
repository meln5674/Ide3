module Ide3.NewMonad.Instances.State.SolutionClass where

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types.Internal

instance StatefulSolutionClass m => SolutionClass (StatefulWrapper m) where
    editSolutionInfo f = modifySolution $ \s -> s{ solutionInfo = f $ solutionInfo s }
    addProject a = modifySolutionER $ \s -> runDescent2 Solution.addProject s a
    removeProject a = modifySolutionER $ \s -> runDescent2 Solution.removeProject s a
    getProjects = modifySolutionER $ \s -> runDescent1 Solution.getProjects s
    editProjectInfo a b = modifySolutionER $ \s -> runDescent3 Solution.editProjectInfo s a b
