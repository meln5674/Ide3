{-|
Module      : ProjectRemover
Description : retreiving initialized projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A ProjectRemover is an abstract data type which attempts to retrieve the argument used to initialize a project
-}
{-# LANGUAGE RankNTypes #-}
module ProjectRemover
    ( ProjectRemoverResult (..)
    , ProjectRemover (..)
    , runProjectRemover
    , noProjectRemover
    , mapProjectRemover
    ) where

import Control.Monad.Trans.Except

import Ide3.Types

-- | The result of initialization
data ProjectRemoverResult
    = ProjectRemoverSucceeded String String
    | ProjectRemoverFailed String String

-- | The Remover abstract type. Use runRemover or runRemoverWithInput
-- to execute the actions of an Remover
data ProjectRemover a m = ProjectRemover
    { runProjectRemoverInternal :: forall u . a -> SolutionResult u m ProjectRemoverResult }

-- | Run an Remover with its arguments
runProjectRemover :: ProjectRemover a m
                    -> a
                    -> SolutionResult u m ProjectRemoverResult
runProjectRemover = runProjectRemoverInternal

-- | An Remover that represents no initialization capability, and will
-- always result in an error
noProjectRemover :: Monad m => ProjectRemover a m
noProjectRemover = ProjectRemover $ \_ -> throwE $ Unsupported "No project Remover specified"


mapProjectRemover :: (forall b . m b -> m' b) 
                    -> ProjectRemover a m 
                    -> ProjectRemover a m'
mapProjectRemover f (ProjectRemover b) = ProjectRemover (\x -> mapExceptT f $ b x) 

