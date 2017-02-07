{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
module GuiEnv where

import Control.Concurrent
import Control.Monad.STM

import Control.Concurrent.STM.TChan

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Ide3.Utils

import GuiHelpers
import Viewer
import GuiMonad

import GuiViewer
import GuiViewer.Class

import GenericGuiEnv (IdleThreadTask(..))

type MVarType p = (GuiViewerState, (ViewerState, p))

data GuiEnv m p
    = GuiEnv
    { guiComponents :: GuiComponents
    , projectMVar :: MVar (MVarType p)
    , idleQueue :: TChan IdleThreadTask
    }

addIdleTask :: MonadIO m => IdleThreadTask -> GuiEnvT m' p m ()
addIdleTask task = do
    env <- getEnv
    liftIO $ atomically $ writeTChan (idleQueue env) task

runIdleThread :: MonadIO m => GuiEnvT m' p m ()
runIdleThread = do
    env <- getEnv
    result <- liftIO $ atomically $ tryReadTChan $ idleQueue env
    case result of
        Just task -> liftIO $ getIdleThreadTask task
        Nothing -> return ()

withSolutionMVar :: (Monad m)
                => (MVar (MVarType p) -> GuiEnvT m' p m a)
                -> GuiEnvT m' p m a
withSolutionMVar f = getEnv >>= f . projectMVar 

withGuiComponents :: (Monad m)
            => (GuiComponents  -> GuiEnvT m' p m a)
            -> GuiEnvT m' p m a
withGuiComponents f = getEnv >>= f . guiComponents

newtype GuiEnvT (m' :: * -> *) p m a 
    = GuiEnvT { runGuiEnvTInternal :: ReaderT (GuiEnv  m' p) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadBounce
    , MonadSplice
    , MonadUnsplice
    , SignalInterceptClass
    )

instance (GuiViewerClass m) => GuiViewerClass (GuiEnvT m' p m) where
    setSearchMode = lift . setSearchMode
    getSearchMode = lift getSearchMode
    openDeclaration = lift .-.. openDeclaration
    getOpenDeclarations = lift getOpenDeclarations
    getOpenDeclaration = lift . getOpenDeclaration
    replaceHistoryPath = lift . replaceHistoryPath
    replaceHistoryText = lift . replaceHistoryText
    navigateHistoryBack = lift navigateHistoryBack
    navigateHistoryForward = lift navigateHistoryForward
    updateHistoryPath = lift .-.. updateHistoryPath

runGuiEnvT :: GuiEnvT m' p m a -> GuiEnv  m' p  -> m a
runGuiEnvT = runReaderT . runGuiEnvTInternal

mkGuiEnvT :: (GuiEnv  m' p  -> m a) -> GuiEnvT m' p m a
mkGuiEnvT = GuiEnvT . ReaderT

getEnv :: (Monad m) => GuiEnvT m' p m (GuiEnv  m' p )
getEnv = GuiEnvT $ ask

mapGuiEnv :: (m1 a -> m2 b) 
          -> GuiEnvT m' p m1 a 
          -> GuiEnvT m' p m2 b
mapGuiEnv f m = GuiEnvT $ mapReaderT f $ runGuiEnvTInternal m 
