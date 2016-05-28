{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module PseudoState where

import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State as Lazy

import Ide3.Types
import Ide3.Mechanism.State

class PseudoStateT t s | t -> s where
    runPseudoStateT :: (Monad m) => t m a -> s -> m (a, s)


instance PseudoStateT (Strict.StateT s) s where
    runPseudoStateT = Strict.runStateT

instance PseudoStateT (Lazy.StateT s) s where
    runPseudoStateT = Lazy.runStateT

instance PseudoStateT ProjectStateT Project where
    runPseudoStateT = runProjectStateT
