{-|
Module      : Ide3.NewMonad.Instance.State.Class.Instances.Lazy
Description : Lazy stateful implementation of NewMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.State.Class.Instances.Lazy where

import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

import Ide3.Types.State
import Ide3.NewMonad.Instances.State.Class

-- | Wrapper around a lazy StateT Solution
newtype SolutionStateT m a = SolutionStateT
    { runSolutionStateT :: StateT Solution m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- | 
instance Monad m => StatefulSolutionClass (SolutionStateT m) where
    getSolution = lift $ SolutionStateT get
    putSolution = lift . SolutionStateT . put
