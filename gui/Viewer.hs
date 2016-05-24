{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Viewer
Description : Viewer for the demo project
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module defines a monad transformer, ViewerStateT, which adds access to the
state of the program
-}
module Viewer 
    ( module Viewer
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
import Ide3.Types (Project, ProjectError (..), ModuleInfo, DeclarationInfo)

import ViewerMonad

-- | The state of the program
data ViewerState = Viewer { currentModule :: Maybe ModuleInfo, currentDecl :: Maybe DeclarationInfo }

-- | Transformer which adds access to the state of the program
type ViewerStateT = StateT ViewerState

-- | The ViewerStateT transformer applied to another transformer, applied to
-- the project state transformer applied to IO
type ViewerStateM fsp t = ViewerStateT (t (ProjectStateT IO))

-- | A token which can be used to resume the program
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
    createNewFile x = ExceptT $ lift $ runExceptT $ createNewFile x
    createNewDirectory x = ExceptT $ lift $ runExceptT $ createNewDirectory x
    prepareBuild = ExceptT $ lift $ runExceptT $ prepareBuild


-- | Run the viewer state transformer with a given state
runViewerStateT :: Monad m => ViewerStateT m a -> ViewerState -> m (a,ViewerState)
runViewerStateT = runStateT

-- | Run the viewer state transformer with the initial program state
runNewViewerStateT :: Monad m => ViewerStateT m a -> m (a,ViewerState)
runNewViewerStateT = flip runViewerStateT $ Viewer Nothing Nothing

runViewerState :: (MonadIO (t (ProjectStateT IO)))
               => (forall b . t (ProjectStateT IO) b -> fsp -> ProjectStateT IO (b, fsp))
               -> fsp 
               -> ViewerStateM fsp t a 
               -> IO (a,ViewerResume fsp)
runViewerState runFSPT unopened f = resumeViewerState 
    f 
    runFSPT
    (Resume (Viewer Nothing Nothing) unopened initialProject)

-- | Resume the viewer state transformer
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

-- | Check if the program currently has a module open
hasCurrentModule :: ViewerMonad m => ViewerStateT m Bool
hasCurrentModule = liftM isJust $ gets currentModule

-- | Open a project at a given path
openProject :: (MonadIO m, ViewerMonad m, ProjectStateM m, ProjectShellM m)
            => FilePath 
            -> ProjectResult (ViewerStateT m) u ()
openProject path = do
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            setFileToOpen path
            lift $ modify $ \s -> s{currentModule=Nothing,currentDecl=Nothing}
            M.load
        (_,True) -> do
            setDirectoryToOpen path
            lift $ modify $ \s -> s{currentModule=Nothing,currentDecl=Nothing}
            M.load
        (_,_) -> throwE $ InvalidOperation (path ++ " does not exist") ""

-- | Save the current project, optionally with a new path to save to
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

setCurrentDecl :: Monad m => ModuleInfo -> DeclarationInfo -> ViewerStateT m ()
setCurrentDecl mi di = put $ Viewer (Just mi) (Just di)

setCurrentModule :: Monad m => ModuleInfo -> ViewerStateT m ()
setCurrentModule mi = put $ Viewer (Just mi) Nothing