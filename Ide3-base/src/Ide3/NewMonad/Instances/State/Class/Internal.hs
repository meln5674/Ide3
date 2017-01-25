{-|
Module      : Ide3.NewMonad.Instances.State.Class.Internal
Description : Classes for the stateful implementation of the NewMonad
                   typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad.Instances.State.Class.Internal where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Types.State

-- | Class of monads which can get and retrieve a Solution
class Monad m => StatefulSolutionClass m where
    -- | Get the Solution
    getSolution :: SolutionResult u m Solution
    -- | Set the Solution
    putSolution :: Solution -> SolutionResult u m ()
    putSolution = modifySolution . const
    -- | Apply a transformation to the Solution
    modifySolution :: (Solution -> Solution) -> SolutionResult u m ()
    modifySolution f = do
        s <- getSolution
        let s' = f s
        putSolution s'
    {-# MINIMAL (getSolution, putSolution) | (getSolution, modifySolution) #-}

-- | Class of monad which can create, load, and save solutions in some manner
class Monad m => StatefulPersistenceClass m where
    -- | Polymorphic token representing instructions on how to save/load, etc
    type StatefulPersistToken m
    -- | Load a Solution
    loadState :: StatefulPersistToken m -> SolutionResult u m Solution
    -- | Create a new Solution
    newState :: SolutionInfo -> SolutionResult u m Solution
    -- | Save a Solution
    finalizeState :: Maybe (StatefulPersistToken m) -> Solution -> SolutionResult u m ()

-- | A no-op wrapper used to deal with overlapping instances
newtype StatefulWrapper m a = StatefulWrapper { runStatefulWrapper :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , StatefulSolutionClass
           , MonadIO
           )
{-
instance StatefulPersistenceClass m => StatefulPersistenceClass (StatefulWrapper m) where
    type StatefulPersistToken (StatefulWrapper m) = StatefulPersistToken m
    loadState tok = splice2 $ loadState tok
    newState tok = splice2 . newState tok
    finalizeState tok = splice2 . finalizeState tok
-}


-- | Apply the wrapper to a monad
instance MonadTrans StatefulWrapper where
    lift = StatefulWrapper
  
-- | Apply a potentially failing transformation to the Solution
modifySolutionER :: StatefulSolutionClass m 
                 => (Solution -> SolutionResult u m (a,Solution))
                 -> SolutionResult u m a
modifySolutionER f = do
    s <- getSolution
    (x,s') <- f s
    putSolution s'
    return x
