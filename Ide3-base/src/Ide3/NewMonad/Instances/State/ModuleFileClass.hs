{-|
Module      : Ide3.NewMonad.Instances.State.ModuleFileClassClass
Description : Stateful implementation of the ModuleFileClassClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.NewMonad.Instances.State.ModuleFileClass where

import Ide3.Utils
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class

import qualified Ide3.Module.Internal as Module

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

-- | Convert a module to file contents by combining the bodies of each of its
-- elements
instance StatefulSolutionClass m => ModuleFileClass (StatefulWrapper m) where
    toFile pi mi = do
        m <- modifySolutionER $ runDescent3 Solution.getModule pi mi
        return $ Module.toFile m
