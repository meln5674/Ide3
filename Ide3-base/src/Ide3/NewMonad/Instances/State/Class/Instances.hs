{-# LANGUAGE FlexibleInstances #-}
module Ide3.NewMonad.Instances.State.Class.Instances where

import Control.Monad.Trans
import Control.Monad.Trans.State

import Ide3.Types
import Ide3.NewMonad.Instances.State.Class

instance Monad m => StatefulSolutionClass (StateT Solution m) where
    getSolution = lift get
    putSolution = lift . put
