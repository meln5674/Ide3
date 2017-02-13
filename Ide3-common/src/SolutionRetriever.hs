{-|
Module      : SolutionRetriever
Description : retreiving initialized Solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A SolutionRetriever is an abstract data type which attempts to retrieve the configuration for a Solution
-}
{-# LANGUAGE RankNTypes #-}
module SolutionRetriever
    ( SolutionRetrieverResult (..)
    , SolutionRetriever (..)
    , runSolutionRetriever
    , noSolutionRetriever
    , mapSolutionRetriever
    ) where

import Control.Monad.Trans.Except

import Ide3.Types

-- | The result of initialization
data SolutionRetrieverResult
    = SolutionRetrieverSucceeded String String SolutionInfo
    | SolutionRetrieverFailed String String

-- | The Retriever abstract type. Use runRetriever or runRetrieverWithInput
-- to execute the actions of an Retriever
data SolutionRetriever a m = SolutionRetriever
    { runSolutionRetrieverInternal :: forall u . SolutionResult u m a }

-- | Run an Retriever with its arguments
runSolutionRetriever :: SolutionRetriever a m
                    -> SolutionResult u m a
runSolutionRetriever = runSolutionRetrieverInternal

-- | An Retriever that represents no initialization capability, and will
-- always result in an error
noSolutionRetriever :: Monad m => SolutionRetriever a m
noSolutionRetriever = SolutionRetriever $ throwE $ Unsupported "No Solution Retriever specified"

mapSolutionRetriever :: (forall b . m b -> m' b) 
                    -> SolutionRetriever a m 
                    -> SolutionRetriever a m'
mapSolutionRetriever f (SolutionRetriever b) = SolutionRetriever (mapExceptT f b) 
