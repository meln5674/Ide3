{-|
Module      : Ide3.NewerMonad.Instances.State.Class
Description : Classes for the stateful implementation of the NewerMonad
                   typeclasses
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ide3.NewerMonad.Instances.State.Class where

import Control.Monad.Trans
import Control.Monad.Except

import Ide3.Types.Internal
import Ide3.Types.State

-- | Class of monads which can get and retrieve a Solution
class (MonadError (SolutionError u) m) => StatefulSolutionClass u m where
    -- | Get the Solution
    getSolution :: m Solution
    -- | Set the Solution
    putSolution :: Solution -> m ()
    putSolution = modifySolution . const
    -- | Apply a transformation to the Solution
    modifySolution :: (Solution -> Solution) -> m ()
    modifySolution f = do
        s <- getSolution
        let s' = f s
        putSolution s'
    {-# MINIMAL (getSolution, putSolution) | (getSolution, modifySolution) #-}

-- | Class of monad which can create, load, and save solutions in some manner
class (MonadError (SolutionError u) m) => StatefulPersistenceClass u m where
    -- | Load a Solution
    loadState :: m Solution
    -- | Create a new Solution
    newState :: SolutionInfo -> m Solution
    -- | Save a Solution
    finalizeState :: Solution -> m ()

-- | A no-op wrapper used to deal with overlapping instances
newtype StatefulWrapper m a = StatefulWrapper { runStatefulWrapper :: m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           )

deriving instance StatefulSolutionClass u m => StatefulSolutionClass u (StatefulWrapper m)
deriving instance StatefulPersistenceClass u m => StatefulPersistenceClass u (StatefulWrapper m)

-- | Apply the wrapper to a monad
instance MonadTrans StatefulWrapper where
    lift = StatefulWrapper
  
-- | Apply a potentially failing transformation to the Solution
modifySolutionER :: StatefulSolutionClass u m 
                 => (Solution -> m (a,Solution))
                 -> m a
modifySolutionER f = do
    s <- getSolution
    (x,s') <- f s
    putSolution s'
    return x
    
