{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.State.Class.Instances.Strict where

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import Ide3.Types.Internal
import Ide3.NewMonad.Instances.State.Class

newtype SolutionStateT m a = SolutionStateT
    { runSolutionStateT :: StateT Solution m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => StatefulSolutionClass (SolutionStateT m) where
    getSolution = lift $ SolutionStateT get
    putSolution = lift . SolutionStateT . put
