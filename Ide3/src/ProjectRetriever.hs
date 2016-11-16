{-|
Module      : ProjectRetriever
Description : retreiving initialized projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A ProjectRetriever is an abstract data type which attempts to retrieve the argument used to initialize a project
-}
{-# LANGUAGE RankNTypes #-}
module ProjectRetriever
    ( ProjectRetrieverResult (..)
    , ProjectRetriever (..)
    , runProjectRetriever
    , noProjectRetriever
    , mapProjectRetriever
    ) where

import System.Exit
import System.Process
import System.Directory
import System.FilePath

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.NewMonad
import Ide3.Digest

import Args
import Viewer

-- | The result of initialization
data ProjectRetrieverResult
    = ProjectRetrieverSucceeded String String ProjectInfo
    | ProjectRetrieverFailed String String

-- | The Retriever abstract type. Use runRetriever or runRetrieverWithInput
-- to execute the actions of an Retriever
data ProjectRetriever a m = ProjectRetriever
    { runProjectRetrieverInternal :: forall u . ProjectInfo -> SolutionResult u m a }

-- | Run an Retriever with its arguments
runProjectRetriever :: (Monad m)
               => ProjectRetriever a m
               -> ProjectInfo
               -> SolutionResult u m a
runProjectRetriever = runProjectRetrieverInternal

-- | An Retriever that represents no initialization capability, and will
-- always result in an error
noProjectRetriever :: Monad m => ProjectRetriever a m
noProjectRetriever = ProjectRetriever $ \_ -> throwE $ Unsupported "No project Retriever specified"

mapProjectRetriever :: (forall a . m a -> m' a) -> ProjectRetriever a m -> ProjectRetriever a m'
mapProjectRetriever f (ProjectRetriever b) = ProjectRetriever (\x -> mapExceptT f $ b x) 
