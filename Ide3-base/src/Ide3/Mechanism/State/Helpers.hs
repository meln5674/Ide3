{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Ide3.Mechanism.State.Helpers where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Identity

import Ide3.Types

import Ide3.Mechanism.State.Types

{-
getsSolution :: SolutionStateM m => (Solution -> a) -> m a
getsSolution f = f <$> getSolution

modifySolution :: SolutionStateM m => (Solution -> Solution) -> m ()
modifySolution f = liftM f getSolution >>= putSolution

modifySolutionE :: SolutionStateM m => (Solution -> Either (SolutionError u) Solution) -> SolutionResult m u ()
modifySolutionE f = modifySolutionER $ \p -> do
    p' <- f p
    return (p',())

modifySolutionER :: SolutionStateM m => (Solution -> Either (SolutionError u) (Solution,a)) -> SolutionResult m u a
modifySolutionER f = do
    p <- lift getSolution
    case f p of
        Right (p',r) -> do lift $ putSolution p'
                           return r
        Left msg -> throwE msg
-}

getsSolution :: SolutionStateM m => (Solution -> a) -> m a
getsSolution f = f <$> getSolution

modifySolution :: SolutionStateM m => (Solution -> Solution) -> m ()
modifySolution f = liftM f getSolution >>= putSolution

modifySolutionE :: SolutionStateM m => (Solution -> Either (SolutionError u) Solution) -> SolutionResult m u ()
modifySolutionE f = modifySolutionER $ \p -> do
    p' <- f p
    return (p',())

modifySolutionER :: SolutionStateM m => (Solution -> Either (SolutionError u) (Solution,a)) -> SolutionResult m u a
modifySolutionER f = do
    p <- lift getSolution
    case f p of
        Right (p',r) -> do lift $ putSolution p'
                           return r
        Left msg -> throwE msg


modifySolutionEnv :: SolutionStateM m 
                  => (Solution -> ExceptT (SolutionError u) m (a,Solution))
                  -> SolutionResult m u a
modifySolutionEnv f = do
    s <- lift $ getSolution
    result <- lift $ runExceptT $ f s
    case result of
        Right (x,s') -> do
            lift $ putSolution s'
            return x
        Left e -> throwE e
