{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.State.Class where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.NewMonad

import Ide3.Types

class Monad m => StatefulSolutionClass m where
    getSolution :: SolutionResult m u Solution
    putSolution :: Solution -> SolutionResult m u ()
    putSolution = modifySolution . const
    modifySolution :: (Solution -> Solution) -> SolutionResult m u ()
    modifySolution f = do
        s <- getSolution
        let s' = f s
        putSolution s'
    {-# MINIMAL (getSolution, putSolution) | (getSolution, modifySolution) #-}

class Monad m => StatefulPersistenceClass m where
    loadState :: SolutionResult m u Solution
    newState :: SolutionInfo -> SolutionResult m u Solution
    finalizeState :: Solution -> SolutionResult m u ()

newtype StatefulWrapper m a = StatefulWrapper { runStatefulWrapper :: m a }
  deriving (Functor, Applicative, Monad, StatefulSolutionClass, StatefulPersistenceClass, MonadIO)
  
instance MonadTrans StatefulWrapper where
    lift = StatefulWrapper


instance (StatefulSolutionClass m, StatefulPersistenceClass m) => PersistenceClass (StatefulWrapper m) where
    load = loadState >>= putSolution
    new = newState >=> putSolution
    finalize = getSolution >>= finalizeState

modifySolutionER :: StatefulSolutionClass m 
                 => (Solution -> SolutionResult m u (a,Solution))
                 -> SolutionResult m u a
modifySolutionER f = do
    s <- getSolution
    (x,s') <- f s
    putSolution s'
    return x
    
