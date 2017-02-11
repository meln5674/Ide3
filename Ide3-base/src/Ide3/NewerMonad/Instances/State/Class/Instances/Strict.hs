{-|
Module      : Ide3.NewerMonad.Instance.State.Class.Instances.Strict
Description : Strict stateful implementation of NewerMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewerMonad.Instances.State.Class.Instances.Strict where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import Control.Monad.Except

import Ide3.Types
import Ide3.Types.State
import Ide3.NewerMonad.Instances.State.Class

-- | Wrapper around a stricft StateT Solution
newtype SolutionStateT m a = SolutionStateT
    { runSolutionStateT :: StateT Solution m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | Use state monad to get and put solution strictly
instance (MonadError (SolutionError u) m) => StatefulSolutionClass u (SolutionStateT m) where
    getSolution = lift $ SolutionStateT get
    putSolution = lift . SolutionStateT . put
