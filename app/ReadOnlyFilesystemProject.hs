{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ReadOnlyFilesystemProject where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Mechanism.State
import Ide3.Monad (ProjectM)
import Ide3.Types (Project, ProjectError (..))

import ViewerMonad

import Digest


data FileSystemProject
    = ToOpen FilePath
    | Unopened
    | Opened FilePath
    deriving Show

type ReadOnlyFilesystemProjectT = StateT FileSystemProject

runReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> FileSystemProject -> m (a, FileSystemProject)
runReadOnlyFilesystemProjectT = runStateT

runNewReadOnlyFilesystemProjectT :: MonadIO m => ReadOnlyFilesystemProjectT m a -> m (a, FileSystemProject)
runNewReadOnlyFilesystemProjectT = flip runReadOnlyFilesystemProjectT Unopened

instance ProjectStateM m => ProjectStateM (ReadOnlyFilesystemProjectT m) where
    getProject = lift getProject
    putProject = lift . putProject

instance MonadIO m => ProjectShellM (ReadOnlyFilesystemProjectT m) where
    new _ = throwE $ Unsupported "Cannot create a new read-only project"
    load = do
        fsp <- lift get
        case fsp of
            ToOpen path -> do
                p <- digestProject' path (Just "ifaces")
                lift $ put $ Opened path
                return p
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened path -> digestProject' path (Just "ifaces")
    finalize _ = throwE $ Unsupported "Cannot save a read-only project"


instance (MonadIO m, ProjectStateM m) => ViewerMonad (ReadOnlyFilesystemProjectT m) where
    setFileToOpen x = throwE $ Unsupported "Cannot open a file in a readonly project"
    setDirectoryToOpen x = lift $ put $ ToOpen x
    setTargetPath x = throwE $ Unsupported "Cannot set a target path for a readonly project"
    hasOpenedProject = do
        fsp <- get
        case fsp of
            Opened _ -> return True
            _ -> return False
