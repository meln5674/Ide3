{-|
Module      : ProjectEditor
Description : retreiving initialized projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A ProjectEditor is an abstract data type which edits a probject that was previously initialized
-}
{-# LANGUAGE RankNTypes #-}
module ProjectEditor
    ( ProjectEditorResult (..)
    , ProjectEditor (..)
    , runProjectEditor
    , runProjectEditorWithInput
    , noProjectEditor
    , mapProjectEditor
    ) where

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types

import Args

-- | The result of initialization
data ProjectEditorResult
    = ProjectEditorSucceeded String String ProjectInfo
    | ProjectEditorFailed String String

-- | The Editor abstract type. Use runEditor or runEditorWithInput
-- to execute the actions of an Editor
data ProjectEditor a m = ProjectEditor
    { runProjectEditorInternal :: forall u . ProjectInfo -> a -> SolutionResult u m ProjectEditorResult }

-- | Run an Editor with a list of strings to parse into arguments
runProjectEditorWithInput :: (Args a)
                          => ProjectEditor a m
                          -> ProjectInfo
                          -> [String]
                          -> Either String 
                            (SolutionResult u m ProjectEditorResult)
runProjectEditorWithInput editor pji = liftM (runProjectEditorInternal editor pji) . getArgsFrom

-- | Run an Editor with its arguments
runProjectEditor :: ProjectEditor a m
                 -> ProjectInfo
                 -> a
                 -> SolutionResult u m ProjectEditorResult
runProjectEditor = runProjectEditorInternal

-- | An Editor that represents no initialization capability, and will
-- always result in an error
noProjectEditor :: Monad m => ProjectEditor a m
noProjectEditor = ProjectEditor $ \_ _ -> throwE $ Unsupported "No project Editor specified"

mapProjectEditor :: (forall b . m b -> m' b) 
                 -> ProjectEditor a m 
                 -> ProjectEditor a m'
mapProjectEditor f (ProjectEditor b) 
    = ProjectEditor (\x y -> mapExceptT f $ b x y)
