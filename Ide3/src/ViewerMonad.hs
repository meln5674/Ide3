{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
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
module ViewerMonad where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Types

import Ide3.Monad
import Ide3.Mechanism.State -- (ProjectStateM (..), ProjectShellM (..))
--import Ide3.Mechanism.State

import PseudoState

class (ProjectM m) => ViewerMonad m where
    -- | Set the file to open so that when ProjectM.load is called, that path is
    -- used to open a project
    setFileToOpen :: FilePath -> ProjectResult m u ()
    -- | Set the directory to open so that when ProjectM.load is called, that
    -- path is used to open a project
    setDirectoryToOpen :: FilePath -> ProjectResult m u ()
    -- | Set the path to save to so that when ProjectM.finalize is called, that
    -- path is used to save the project
    setTargetPath ::  String -> ProjectResult m u ()
    -- | Check if there is a project currently open
    hasOpenedProject :: m Bool
    -- | Set the project to an empty project at the provided file path
    createNewFile :: FilePath -> ProjectResult m u ()
    -- | Set the project to an empty project at the provided directory path
    createNewDirectory :: FilePath -> ProjectResult m u ()
    -- | Perform any actions necessary to be able to build using the 'stack build' shell command
    prepareBuild :: ProjectResult m u ()

{-
instance (PseudoStateT t s, ProjectM (t m), MonadTrans t, ViewerMonad m) => ViewerMonad (t m) where
    setFileToOpen x = ExceptT $ lift $ runExceptT $ setFileToOpen x
    setDirectoryToOpen x = ExceptT $ lift $ runExceptT $ setDirectoryToOpen x
    setTargetPath x = ExceptT $ lift $ runExceptT $ setTargetPath x
    hasOpenedProject = lift hasOpenedProject
    createNewFile x = ExceptT $ lift $ runExceptT $ createNewFile x
    createNewDirectory x = ExceptT $ lift $ runExceptT $ createNewDirectory x
    prepareBuild = ExceptT $ lift $ runExceptT prepareBuild
-}

instance (ViewerMonad m) => ViewerMonad (StateT s m) where
    setFileToOpen x = ExceptT $ lift $ runExceptT $ setFileToOpen x
    setDirectoryToOpen x = ExceptT $ lift $ runExceptT $ setDirectoryToOpen x
    setTargetPath x = ExceptT $ lift $ runExceptT $ setTargetPath x
    hasOpenedProject = lift hasOpenedProject
    createNewFile x = ExceptT $ lift $ runExceptT $ createNewFile x
    createNewDirectory x = ExceptT $ lift $ runExceptT $ createNewDirectory x
    prepareBuild = ExceptT $ lift $ runExceptT prepareBuild


