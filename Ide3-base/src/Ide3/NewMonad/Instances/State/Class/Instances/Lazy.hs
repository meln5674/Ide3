{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.State.Class.Instances.Lazy where

import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy

import Ide3.Types
import Ide3.NewMonad.Instances.State.Class

newtype SolutionStateT m a = SolutionStateT
    { runSolutionStateT :: StateT Solution m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance Monad m => StatefulSolutionClass (SolutionStateT m) where
    getSolution = lift $ SolutionStateT $ get
    putSolution = lift . SolutionStateT . put
