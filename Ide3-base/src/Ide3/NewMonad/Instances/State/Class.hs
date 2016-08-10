{-|
Module      : Ide3.NewMonad.Instances.State.Class
Description : Classes for the stateful implementation of the NewMonad typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ide3.NewMonad.Instances.State.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Types.State

-- | Class of monads which can get and retreive a Solution
class Monad m => StatefulSolutionClass m where
    -- | Get the Solution
    getSolution :: SolutionResult m u Solution
    -- | Set the Solution
    putSolution :: Solution -> SolutionResult m u ()
    putSolution = modifySolution . const
    -- | Apply a transformation to the Solution
    modifySolution :: (Solution -> Solution) -> SolutionResult m u ()
    modifySolution f = do
        s <- getSolution
        let s' = f s
        putSolution s'
    {-# MINIMAL (getSolution, putSolution) | (getSolution, modifySolution) #-}

-- | Class of monad which can create, load, and save solutions in some manner
class Monad m => StatefulPersistenceClass m where
    -- | Load a Solution
    loadState :: SolutionResult m u Solution
    -- | Create a new Solution
    newState :: SolutionInfo -> SolutionResult m u Solution
    -- | Save a Solution
    finalizeState :: Solution -> SolutionResult m u ()

-- | A no-op wrapper used to deal with overlapping instances
newtype StatefulWrapper m a = StatefulWrapper { runStatefulWrapper :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , StatefulSolutionClass
           , StatefulPersistenceClass
           , MonadIO
           )
  
-- | Apply a potentially failing transformation to the Solution
modifySolutionER :: StatefulSolutionClass m 
                 => (Solution -> SolutionResult m u (a,Solution))
                 -> SolutionResult m u a
modifySolutionER f = do
    s <- getSolution
    (x,s') <- f s
    putSolution s'
    return x
    
