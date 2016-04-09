{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Viewer where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State


import Ide3.Mechanism.State

import Digest

data ViewerState = Viewer { currentModule :: Maybe String }

data FileSystemProject
    = ToOpen FilePath
    | Unopened
    | Opened FilePath

type ViewerStateM = StateT ViewerState (StateT FileSystemProject (ProjectStateT IO))

runViewerState :: ViewerStateM a -> IO a
runViewerState f = result
  where
    runViewer = evalStateT f (Viewer Nothing)
    runFPS = evalStateT runViewer Unopened
    runProject = runProjectStateT runFPS
    result = fst <$> runProject

instance ProjectStateM ViewerStateM where
    getProject = lift (lift get)
    putProject p = lift (lift (put p))

instance ProjectShellM ViewerStateM where
    new _ = throwE $ "Creating new projects is unsupported at this time"
    load = do
        fsp <- lift (lift get)
        case fsp of
            ToOpen path -> digestProject' path
            Unopened -> throwE $ "No path specified for opening"
            Opened path -> digestProject' path
    finalize _ = throwE $ "Saving projects is unsupported at this time"
