{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Viewer 
    ( module Viewer
    , FileSystemProject (..)
    , module ViewerMonad
    ) where

import Data.Maybe

import System.Directory

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Mechanism.State
import Ide3.Monad hiding (load, new, finalize)
import qualified Ide3.Monad as M
import Ide3.Types (Project, ProjectError (..))


import ReadOnlyFilesystemProject

import ViewerMonad

data ViewerState = Viewer { currentModule :: Maybe String }

type ViewerStateT = StateT ViewerState

type ViewerStateM fsp t = ViewerStateT (t (ProjectStateT IO))

data ViewerResume fsp = Resume ViewerState fsp Project


instance ProjectStateM m => ProjectStateM (ViewerStateT m) where
    getProject = lift getProject
    putProject = lift . putProject

instance ProjectShellM m => ProjectShellM (ViewerStateT m) where
    new x = ExceptT $ lift $ runExceptT $ new x
    load = ExceptT $ lift $ runExceptT $ load
    finalize x = ExceptT $ lift $ runExceptT $ finalize x



instance (ProjectStateM m, ProjectShellM m, ViewerMonad m) => ViewerMonad (ViewerStateT m) where
    setFileToOpen x = ExceptT $ lift $ runExceptT $ setFileToOpen x
    setDirectoryToOpen x = ExceptT $ lift $ runExceptT $ setDirectoryToOpen x
    setTargetPath x = ExceptT $ lift $ runExceptT $ setTargetPath x
    hasOpenedProject = lift hasOpenedProject
--    saveCurrentProject = ExceptT $ lift $ runExceptT $ saveCurrentProject


runViewerStateT :: Monad m => ViewerStateT m a -> ViewerState -> m (a,ViewerState)
runViewerStateT = runStateT

runNewViewerStateT :: Monad m => ViewerStateT m a -> m (a,ViewerState)
runNewViewerStateT = flip runViewerStateT $ Viewer Nothing

runViewerState :: (MonadIO (t (ProjectStateT IO)))
               => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
               -> fsp 
               -> ViewerStateM fsp t a 
               -> IO (a,ViewerResume fsp)
runViewerState runFSPT unopened f = resumeViewerState 
    f 
    runFSPT
    (Resume (Viewer Nothing) unopened initialProject)
{-    let runViewer = runStateT f (Viewer Nothing)
        runFPS = runStateT runViewer Unopened
        runProject = runProjectStateT runFPS
    (((result,viewer),fsp),proj) <- runProject
    return (result,Resume viewer fsp proj)-}

resumeViewerState :: 
                     (MonadIO (t (ProjectStateT IO)))
                  => ViewerStateM fsp t a 
                  -> (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b,fsp))
                  -> ViewerResume fsp 
                  -> IO (a,ViewerResume fsp)
resumeViewerState f runFSPT (Resume viewer fsp proj) = do
    let runViewer = runViewerStateT f viewer
        runFSP = runFSPT runViewer fsp
        runProject = runProjectStateT runFSP proj
    (((result,viewer'),fsp'),proj') <- runProject
    return (result,Resume viewer' fsp' proj')

--hasCurrentModule :: (Monad (t (ProjectStateT IO))) => ViewerStateM fsp t Bool
hasCurrentModule :: ViewerMonad m => ViewerStateT m Bool
hasCurrentModule = liftM isJust $ gets currentModule

openProject :: (MonadIO m, ViewerMonad m, ProjectStateM m, ProjectShellM m)
            => FilePath 
            -> ProjectResult (ViewerStateT m) u ()
openProject path = do
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            setFileToOpen path
            lift $ modify $ \s -> s{currentModule=Nothing}
            M.load
        (_,True) -> do
            setDirectoryToOpen path
            lift $ modify $ \s -> s{currentModule=Nothing}
            M.load
        (_,_) -> throwE $ InvalidOperation (path ++ " does not exist") ""

saveProject :: (MonadIO m, ViewerMonad m, ProjectStateM m, ProjectShellM m) 
            => (Maybe FilePath)
            -> ProjectResult (ViewerStateT m) u ()
saveProject maybePath = do
    cond <- lift hasOpenedProject
    if cond
        then do 
                case maybePath of
                    Just path -> setTargetPath path
                    Nothing -> return ()
                M.finalize
        else throwE $ InvalidOperation "No project is currently open" ""
