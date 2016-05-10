{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : ReadOnlyFilesystemProject
Description : Read only persistence mechanism
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This modules provides a "persistance" mechanism which only permits the digesting
of existing standard haskell projects and then viewing/searching them.

Attempting to save/edit/create using this persistence mechanism will result in an error
-}
module ReadOnlyFilesystemProject where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Mechanism.State
import Ide3.Types (ProjectError (..))

import ViewerMonad

import Digest

-- | State of the mechanism
data FileSystemProject
    -- | There is a path to be opened
    = ToOpen FilePath
    -- | No project opened
    | Unopened
    -- | A project at the path is opened
    | Opened FilePath
    deriving Show

-- | State transformer for the mechanism
type ReadOnlyFilesystemProjectT = StateT FileSystemProject

-- | Run an action inside the mechanism with the provided state
runReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> FileSystemProject -> m (a, FileSystemProject)
runReadOnlyFilesystemProjectT = runStateT

-- | Run an action inside the mechanism 
runNewReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> m (a, FileSystemProject)
runNewReadOnlyFilesystemProjectT = flip runReadOnlyFilesystemProjectT Unopened

instance ProjectStateM m => ProjectStateM (ReadOnlyFilesystemProjectT m) where
    getProject = lift getProject
    putProject = lift . putProject

instance MonadIO m => ProjectShellM (ReadOnlyFilesystemProjectT m) where
    -- | Not supported
    new _ = throwE $ Unsupported "Cannot create a new read-only project"
    -- | Digest a project after loading the interface file
    load = do
        fsp <- lift get
        case fsp of
            ToOpen path -> do
                p <- digestProject' path (Just "ifaces")
                lift $ put $ Opened path
                return p
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened path -> digestProject' path (Just "ifaces")
    -- | Not supported
    finalize _ = throwE $ Unsupported "Cannot save a read-only project"


instance (MonadIO m, ProjectStateM m) => ViewerMonad (ReadOnlyFilesystemProjectT m) where
    -- | Not supported
    setFileToOpen _ = throwE $ Unsupported "Cannot open a file in a readonly project"
    -- | Set the path to be digested
    setDirectoryToOpen x = lift $ put $ ToOpen x
    -- | Unsupported
    setTargetPath _ = throwE $ Unsupported "Cannot set a target path for a readonly project"
    -- | Check if a project has been digested
    hasOpenedProject = do
        fsp <- get
        case fsp of
            Opened _ -> return True
            _ -> return False
    createNewFile _ = throwE $ Unsupported "Cannot create a new readonly project"
    createNewDirectory _ = throwE $ Unsupported "Cannot create a new readonly project"
    prepareBuild = return ()
