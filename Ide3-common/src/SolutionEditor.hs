{-|
Module      : SolutionEditor
Description : retreiving initialized Solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A SolutionEditor is an abstract data type which edits a solution that was previously initialized
-}
{-# LANGUAGE RankNTypes #-}
module SolutionEditor
    ( SolutionEditorResult (..)
    , SolutionEditor (..)
    , runSolutionEditor
    , runSolutionEditorWithInput
    , noSolutionEditor
    , mapSolutionEditor
    ) where

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types

import Args

-- | The result of initialization
data SolutionEditorResult
    = SolutionEditorSucceeded String String
    | SolutionEditorFailed String String

-- | The Editor abstract type. Use runEditor or runEditorWithInput
-- to execute the actions of an Editor
data SolutionEditor a m = SolutionEditor
    { runSolutionEditorInternal :: forall u . a -> SolutionResult u m SolutionEditorResult }

-- | Run an Editor with a list of strings to parse into arguments
runSolutionEditorWithInput :: (Args a)
                          => SolutionEditor a m
                          -> [String]
                          -> Either String 
                            (SolutionResult u m SolutionEditorResult)
runSolutionEditorWithInput editor = fmap (runSolutionEditorInternal editor) . getArgsFrom

-- | Run an Editor with its arguments
runSolutionEditor :: SolutionEditor a m
                 -> a
                 -> SolutionResult u m SolutionEditorResult
runSolutionEditor = runSolutionEditorInternal

-- | An Editor that represents no initialization capability, and will
-- always result in an error
noSolutionEditor :: Monad m => SolutionEditor a m
noSolutionEditor = SolutionEditor $ \_ -> throwE $ Unsupported "No Solution Editor specified"

mapSolutionEditor :: (forall b . m b -> m' b) 
                 -> SolutionEditor a m 
                 -> SolutionEditor a m'
mapSolutionEditor f (SolutionEditor b) 
    = SolutionEditor (\x -> mapExceptT f $ b x)
