{-|
Module      : Ide3.Mechanism.State.Helpers
Description : Utilities used for the stateful solution monad
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
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ide3.Mechanism.State.Helpers where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Types

import Ide3.Mechanism.State.Types

-- | Retreive an in-memory solution and return the result of a pure
-- transformation on it
getsSolution :: SolutionStateM m => (Solution -> a) -> m a
getsSolution f = f <$> getSolution

-- | Modify an in-memory solution using a pure transformation
modifySolution :: SolutionStateM m => (Solution -> Solution) -> m ()
modifySolution f = liftM f getSolution >>= putSolution

-- | Modify an in-memory solution using a transformation that can throw
-- exceptions and returns an additional value
modifySolutionEnv :: SolutionStateM m 
                  => (Solution -> ExceptT (SolutionError u) m (a,Solution))
                  -> SolutionResult u m a
modifySolutionEnv f = do
    s <- lift getSolution
    result <- lift $ runExceptT $ f s
    case result of
        Right (x,s') -> do
            lift $ putSolution s'
            return x
        Left e -> throwE e
