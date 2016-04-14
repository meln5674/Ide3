{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Viewer where

import Data.Maybe

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Ide3.Mechanism.State
import Ide3.Types (Project, ExternExport (..), ExternModule (..), Symbol (..), ModuleInfo (..))

import Digest

data ViewerState = Viewer { currentModule :: Maybe String }

data FileSystemProject
    = ToOpen FilePath
    | Unopened
    | Opened FilePath
    deriving Show

type ViewerStateM = StateT ViewerState (StateT FileSystemProject (ProjectStateT IO))

data ViewerResume = Resume ViewerState FileSystemProject Project

runViewerState :: ViewerStateM a -> IO (a,ViewerResume)
runViewerState f = resumeViewerState f (Resume (Viewer Nothing) (Unopened) initialProject)
{-    let runViewer = runStateT f (Viewer Nothing)
        runFPS = runStateT runViewer Unopened
        runProject = runProjectStateT runFPS
    (((result,viewer),fsp),proj) <- runProject
    return (result,Resume viewer fsp proj)-}

resumeViewerState :: ViewerStateM a -> ViewerResume -> IO (a,ViewerResume)
resumeViewerState f (Resume viewer fsp proj) = do
    let runViewer = runStateT f viewer
        runFSP = runStateT runViewer fsp
        runProject = runStateT runFSP proj
    (((result,viewer'),fsp'),proj') <- runProject
    return (result,Resume viewer' fsp' proj')

hasOpenedProject :: ViewerStateM Bool
hasOpenedProject = do
    fsp <- lift get
    case fsp of
        Opened _ -> return True
        _ -> return False

hasCurrentModule :: ViewerStateM Bool
hasCurrentModule = liftM isJust $ gets currentModule

instance ProjectStateM ViewerStateM where
    getProject = lift (lift get)
    putProject p = lift (lift (put p))

instance ProjectShellM ViewerStateM where
    new _ = throwE $ "Creating new projects is unsupported at this time"
    load = do
        fsp <- lift (lift get)
        case fsp of
            ToOpen path -> catchE (digestProject' path) (\e -> throwE $ "Digest Error: " ++ e)
            Unopened -> throwE $ "No path specified for opening"
            Opened path -> digestProject' path
    finalize _ = throwE $ "Saving projects is unsupported at this time"

