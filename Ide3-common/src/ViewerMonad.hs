{-|
Module      : ViewerMonad
Description : Abstract interface for persistence mechanisms
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module provides the ViewerMonad class, which is an abstract interface
for persistence mechanisms usable by the demo IDE.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module ViewerMonad where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Types
import Ide3.Utils

-- | Provides methods for features of a persistance mechanism
class Monad m => ViewerMonad m where
    type ViewerPersistToken m
    preparePathToken :: FilePath -> m (ViewerPersistToken m)
{-
    -- | Set the file to open so that when SolutionM.load is called, that path is
    -- used to open a project
    setFileToOpen :: FilePath -> SolutionResult u m ()
    -- | Set the directory to open so that when SolutionM.load is called, that
    -- path is used to open a project
    setDirectoryToOpen :: FilePath -> SolutionResult u m ()
    -- | Set the path to save to so that when SolutionM.finalize is called, that
    -- path is used to save the project
    setTargetPath ::  String -> SolutionResult u m ()
-}
    -- | Check if there is a project currently open
    hasOpenedSolution :: m Bool
{-
    -- | Set the project to an empty project at the provided file path
    createNewFile :: FilePath -> SolutionResult u m ()
    -- | Set the project to an empty project at the provided directory path
    createNewDirectory :: FilePath -> SolutionResult u m ()
-}
    -- | Perform any actions necessary to be able to build using the 'stack build' shell command
    prepareBuild :: SolutionResult u m ()

{-
instance (PseudoStateT t s, SolutionM (t m), MonadTrans t, ViewerMonad m) => ViewerMonad (t m) where
    setFileToOpen x = ExceptT $ lift $ runExceptT $ setFileToOpen x
    setDirectoryToOpen x = ExceptT $ lift $ runExceptT $ setDirectoryToOpen x
    setTargetPath x = ExceptT $ lift $ runExceptT $ setTargetPath x
    hasOpenedProject = lift hasOpenedProject
    createNewFile x = ExceptT $ lift $ runExceptT $ createNewFile x
    createNewDirectory x = ExceptT $ lift $ runExceptT $ createNewDirectory x
    prepareBuild = ExceptT $ lift $ runExceptT prepareBuild
-}



instance (ViewerMonad m) => ViewerMonad (StateT s m) where
    type ViewerPersistToken (StateT s m) = ViewerPersistToken m
    preparePathToken x = lift (preparePathToken x)
{-
    setFileToOpen x = ExceptT $ lift $ runExceptT $ setFileToOpen x
    setDirectoryToOpen x = ExceptT $ lift $ runExceptT $ setDirectoryToOpen x
    setTargetPath x = ExceptT $ lift $ runExceptT $ setTargetPath x
-}
    hasOpenedSolution = lift hasOpenedSolution
{-
    createNewFile x = ExceptT $ lift $ runExceptT $ createNewFile x
    createNewDirectory x = ExceptT $ lift $ runExceptT $ createNewDirectory x
-}    
    prepareBuild = splice2 prepareBuild


