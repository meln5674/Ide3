{-|
Module      : Ide3.NewerMonad.Instances.State.Compose
Description : Composition instance for the stateful implementation of the
                NewerMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides an instance of PersistenceClass for any monad that also
has an instance of StatefulSolutionClass and StatefulPersistenceClass using the
obvious method of taking the explicitly returned Solution and using putSolution,
as well as using getSoluiton and providing it explicitly.

The monad in question must be wrapped in a StatefulWrapper to receive this
instance, however, as otherwise this would require UndecidableInstances.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.NewerMonad.Instances.State.Compose where

import Control.Monad

import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

-- | A persistence instance can be created from a stateful solution equipped
-- with the ability to save/load that state
instance ( StatefulSolutionClass u m
         , StatefulPersistenceClass u m
         )
      => PersistenceClass u (StatefulWrapper m) where
    load = loadState >>= putSolution
    new = newState >=> putSolution
    finalize = getSolution >>= finalizeState

