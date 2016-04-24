{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SimpleFilesystemProject where

import Text.Read (readMaybe)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import System.IO.Error

import Ide3.Mechanism.State
import Ide3.Monad (ProjectM)
import Ide3.Types (Project, ProjectError (..))

import qualified Ide3.Project as Project

import ViewerMonad

import Digest

data FileSystemProject
    = ToOpen FilePath
    | ToDigest FilePath
    | Unopened
    | Opened (Maybe FilePath)

type SimpleFilesystemProjectT m = StateT FileSystemProject m

runSimpleFilesystemProjectT :: MonadIO m => SimpleFilesystemProjectT m a -> FileSystemProject -> m (a, FileSystemProject)
runSimpleFilesystemProjectT = runStateT

runNewSimpleFilesystemProjectT :: MonadIO m => SimpleFilesystemProjectT m a -> m (a, FileSystemProject)
runNewSimpleFilesystemProjectT = flip runSimpleFilesystemProjectT Unopened

instance ProjectStateM m => ProjectStateM (SimpleFilesystemProjectT m) where
    getProject = lift getProject
    putProject = lift . putProject

instance MonadIO m => ProjectShellM (SimpleFilesystemProjectT m) where
    new i = do
        lift $ put $ Opened Nothing
        return $ Project.new i
    load = do
        fsp <- lift get
        case fsp of
            ToDigest path -> do
                p <- digestProject' path
                lift $ put $ Opened Nothing
                return p
            ToOpen path -> do
                result <- liftIO $ tryIOError $ readFile path
                case result of
                    Right contents -> case readMaybe contents of
                        Just p -> do
                            lift $ put $ Opened $ Just path
                            return p
                        Nothing -> throwE $ InvalidOperation "File did not contain a valid project" ""
                    Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Unopened -> throwE $ InvalidOperation "No path specified for opening" ""
            Opened Nothing -> throwE $ InvalidOperation "Cannot re-open a digested project" ""
            Opened (Just path) -> do
                lift $ put $ ToOpen path
                load
    finalize p = do
        fsp <- lift get
        case fsp of
            Opened (Just path) -> do
                result <- liftIO $ tryIOError $ writeFile path $ show p
                case result of
                    Right _ -> return ()
                    Left err -> throwE $ InvalidOperation ("Error on writing file: " ++ show err) ""
    finalize _ = throwE $ InvalidOperation ("Cannot finalize a project without a path to write to") ""


instance (MonadIO m, ProjectStateM m) => ViewerMonad (SimpleFilesystemProjectT m) where
    setFileToOpen x = lift $ put $ ToOpen x
    setDirectoryToOpen x = lift $ put $ ToDigest x
    setTargetPath x = throwE $ Unsupported "Cannot set a target path for a readonly project"
    hasOpenedProject = do
        fsp <- get
        case fsp of
            Opened _ -> return True
            _ -> return False
    --saveCurrentProject = 
