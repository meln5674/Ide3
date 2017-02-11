{-|
Module      : Ide3.NewerMonad.Instances.State.ModuleFileClassClass
Description : Stateful implementation of the ModuleFileClassClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ide3.NewerMonad.Instances.State.ModuleFileClass where

import Control.Monad.Except

import Ide3.NewerMonad
import Ide3.NewerMonad.Instances.State.Class

import qualified Ide3.Module.Internal as Module

import Ide3.Env
import qualified Ide3.Env.Solution as Solution

import Ide3.Types

-- | Convert a module to file contents by combining the bodies of each of its
-- elements
instance ( StatefulSolutionClass u (t m)
         , MonadError (SolutionError u) (t m)
         )
      => ModuleFileClass u (t (StatefulWrapper m)) where
    toFile pji mi = do
        m <- modifySolutionER $ runDescent3 Solution.getModule pji mi
        return $ Module.toFile m
