module Ide3.NewMonad.Instances.State.ModuleDeclarationClass where

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

instance StatefulSolutionClass m => ModuleDeclarationClass (StatefulWrapper m) where
    addDeclaration a b c = modifySolutionER $ \s -> runDescent4 Solution.addDeclaration s a b c
    getDeclaration a b c  = modifySolutionER $ \s -> runDescent4 Solution.getDeclaration s a b c
    getDeclarations a b = modifySolutionER $ \s -> runDescent3 Solution.getDeclarations s a b
    editDeclaration a b c d = modifySolutionER $ \s -> runDescent5 Solution.editDeclaration s a b c d
    removeDeclaration a b c = modifySolutionER $ \s -> runDescent4 Solution.removeDeclaration s a b c
