{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ide3.Mechanism.State.Types 
    ( SolutionShellM (..)
    , SolutionStateM (..)
    , StatefulSolution (..)
    , SolutionStateT (..)
    , SolutionState
    ) where


import Control.Monad.Trans
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Types

newtype SolutionStateT m a = SolutionStateT { runSolutionStateTInternal :: StateT Solution m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

type SolutionState = SolutionStateT Identity


class Monad m => SolutionShellM m where
    load :: SolutionResult m u Solution
    new :: SolutionInfo -> SolutionResult m u Solution
    finalize :: Solution -> SolutionResult m u ()

class Monad m => SolutionStateM m where
    getSolution :: m Solution
    putSolution :: Solution -> m ()


newtype StatefulSolution m a = MkStatefulSolution { runStatefulSolution :: m a }
  deriving (Functor, Applicative, Monad, SolutionStateM, SolutionShellM, MonadIO)
