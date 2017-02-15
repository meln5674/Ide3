{-|
Module      : Runner
Description : Running solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Runner is an abstract data type which attempts to run a solution
-}
{-# LANGUAGE RankNTypes #-}
module Runner
    ( Runner (..)
    , RunnerResult (..)
    , runRunner
    , noRunner
    , mapRunner
    )
    where

import Data.Text (Text)

import Control.Monad.Trans.Except

import Ide3.Types

-- | Result of running a solution
data RunnerResult
    = RunFailed Text Text
    | RunSucceeded Text Text

-- | The Runner abstract type. Use runRunner to execute the actions of a runner.
newtype Runner m = MkRunner { runRunnerInternal :: forall u . ProjectInfo -> [String] -> SolutionResult u m RunnerResult }

-- | Execute the actions of a runner
runRunner :: Runner m -> ProjectInfo -> [String] -> SolutionResult u m RunnerResult
runRunner = runRunnerInternal

-- | A Runner that represents no ability to run, and will always result in an error
noRunner :: Monad m => Runner m
noRunner = MkRunner $ \_ _ -> throwE $ Unsupported "No runner specified"

mapRunner :: (forall a . m a -> m' a) -> Runner m -> Runner m'
mapRunner f (MkRunner b) = MkRunner $ \x y -> mapExceptT f $ b x y
