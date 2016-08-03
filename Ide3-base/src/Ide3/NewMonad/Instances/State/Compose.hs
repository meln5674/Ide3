module Ide3.NewMonad.Instances.State.Compose where

import Control.Monad

import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

instance (StatefulSolutionClass m, StatefulPersistenceClass m) => PersistenceClass (StatefulWrapper m) where
    load = loadState >>= putSolution
    new = newState >=> putSolution
    finalize = getSolution >>= finalizeState

