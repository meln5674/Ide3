{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Viewer
Description : Viewer for the demo solution
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module defines a monad transformer, ViewerStateT, which adds access to the
state of the program, and uses the ViewerMonad to wrap around various
funcitonality.
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

import Ide3.Types
import Ide3.Mechanism.State
import Ide3.Monad hiding (load, new, finalize)
import qualified Ide3.Monad as M
import Ide3.Types (Solution, SolutionError (..), DeclarationInfo(..), ModuleInfo(..))

import ViewerMonad

-- | The state of the program
data ViewerState
    = Viewer 
    { currentProject :: Maybe ProjectInfo 
    , currentModule :: Maybe ModuleInfo
    , currentDecl :: Maybe DeclarationInfo 
    }

-- | Transformer which adds access to the state of the program
type ViewerStateT = StateT ViewerState

-- | The ViewerStateT transformer applied to another transformer, applied to
-- the solution state transformer applied to IO
type ViewerStateM fsp t = ViewerStateT (t (SolutionStateT IO))

-- | A token which can be used to resume the program
data ViewerResume fsp = Resume ViewerState fsp Solution


{-
instance SolutionStateM m => SolutionStateM (StateT s m) where
    getSolution = lift getSolution
    putSolution = lift . putSolution
-}

-- | Run the viewer state transformer with a given state
runViewerStateT :: Monad m => ViewerStateT m a -> ViewerState -> m (a,ViewerState)
runViewerStateT = runStateT

-- | Run the viewer state transformer with the initial program state
runNewViewerStateT :: Monad m => ViewerStateT m a -> m (a,ViewerState)
runNewViewerStateT = flip runViewerStateT $ Viewer Nothing Nothing Nothing

runViewerState :: (MonadIO (t (SolutionStateT IO)))
               => (forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b, fsp))
               -> fsp 
               -> ViewerStateM fsp t a 
               -> IO (a,ViewerResume fsp)
runViewerState runFSPT unopened f = resumeViewerState 
    f 
    runFSPT
    (Resume (Viewer Nothing Nothing Nothing) unopened initialSolution)

-- | Resume the viewer state transformer
resumeViewerState :: 
                     (MonadIO (t (SolutionStateT IO)))
                  => ViewerStateM fsp t a 
                  -> (forall b . t (SolutionStateT IO) b -> fsp -> SolutionStateT IO (b,fsp))
                  -> ViewerResume fsp 
                  -> IO (a,ViewerResume fsp)
resumeViewerState f runFSPT (Resume viewer fsp proj) = do
    let runViewer = runViewerStateT f viewer
        runFSP = runFSPT runViewer fsp
        runSolution = runSolutionStateT runFSP proj
    (((result,viewer'),fsp'),proj') <- runSolution
    return (result,Resume viewer' fsp' proj')

-- | Check if the program currently has a module open
--hasCurrentModule :: ViewerMonad m => ViewerStateT m Bool
hasCurrentModule :: (Monad m) => ViewerStateT m Bool
hasCurrentModule = liftM isJust $ gets currentModule


-- | Open a solution at a given path
openSolution :: (MonadIO m, ViewerMonad m)
            => FilePath 
            -> SolutionResult (StateT ViewerState m) u ()
openSolution path = do
    isFile <- liftIO $ doesFileExist path
    isDir <- liftIO $ doesDirectoryExist path
    case (isFile, isDir) of
        (True, _) -> do
            setFileToOpen path
            lift $ modify $ \s -> s{currentProject=Nothing}
            M.load
        (_,True) -> do
            setDirectoryToOpen path
            lift $ modify $ \s -> s{currentProject=Nothing}
            M.load
        (_,_) -> throwE $ InvalidOperation (path ++ " does not exist") ""

-- | Save the current solution, optionally with a new path to save to
saveSolution :: (MonadIO m, ViewerMonad m) 
            => Maybe FilePath
            -> SolutionResult (ViewerStateT m) u ()
saveSolution maybePath = do
    cond <- lift hasOpenedSolution
    if cond
        then do 
                case maybePath of
                    Just path -> setTargetPath path
                    Nothing -> return ()
                M.finalize
        else throwE $ InvalidOperation "No solution is currently open" ""

setCurrentProject :: Monad m => ProjectInfo -> ViewerStateT m ()
setCurrentProject pi = put $ Viewer (Just pi) Nothing Nothing

setCurrentModule :: Monad m => ProjectInfo -> ModuleInfo -> ViewerStateT m ()
setCurrentModule pi mi = put $ Viewer (Just pi) (Just mi) Nothing

-- | Set the current declaration of the program
setCurrentDecl :: Monad m => ProjectInfo -> ModuleInfo -> DeclarationInfo -> ViewerStateT m ()
setCurrentDecl pi mi di = put $ Viewer (Just pi) (Just mi) (Just di)
