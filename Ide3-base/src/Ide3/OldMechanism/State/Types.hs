{-|
Module      : Ide3.Mechanism.State.Types
Description : Types used for the stateful solution monad
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

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

-- | A monad transformer which adds the ability to modify an in-memory solution
newtype SolutionStateT m a = SolutionStateT { runSolutionStateTInternal :: StateT Solution m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

-- | A monad which represents operations on an in-memory solution
type SolutionState = SolutionStateT Identity

-- | Class of monads which can load, create, and save an in-memory solution
class Monad m => SolutionShellM m where
    load :: SolutionResult m u Solution
    new :: SolutionInfo -> SolutionResult m u Solution
    finalize :: Solution -> SolutionResult m u ()

-- | Class of monads which can retreive and overwrite an in-memory solution
class Monad m => SolutionStateM m where
    getSolution :: m Solution
    putSolution :: Solution -> m ()

-- | Wrapper used because of ambiguous typeclass tomfoolery
newtype StatefulSolution m a = MkStatefulSolution { runStatefulSolution :: m a }
  deriving (Functor, Applicative, Monad, SolutionStateM, SolutionShellM, MonadIO)
