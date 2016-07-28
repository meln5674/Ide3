{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : PseudoState
Description : Typeclass for things which behave like StateT
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

Types in the PseudoState class expose a function which takes a value of that
type, some kind of state, and returns a value and an updated state within any monad
-}
module PseudoState where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State as Lazy

import Ide3.Types
import Ide3.Mechanism.State

-- | Typeclass for transformers which behave similarly to StateT
class PseudoStateT t s | t -> s where
    -- | Equivalent of runStateT
    runPseudoStateT :: (Monad m) => t m a -> s -> m (a, s)


instance PseudoStateT (Strict.StateT s) s where
    runPseudoStateT = Strict.runStateT

instance PseudoStateT (Lazy.StateT s) s where
    runPseudoStateT = Lazy.runStateT

instance PseudoStateT SolutionStateT Solution where
    runPseudoStateT = runSolutionStateT
