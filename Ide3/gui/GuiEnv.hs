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

data GuiEnv {-proxy-} m p
    = GuiEnv
    { {-proxy :: proxy m
    ,-} guiComponents :: GuiComponents
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
                => (MVar (MVarType p) -> GuiEnvT {-proxy-} m' p  m a)
                -> GuiEnvT {-proxy-} m' p  m a
withSolutionMVar f = getEnv >>= f . projectMVar 

withGuiComponents :: (Monad m)
            => (GuiComponents  -> GuiEnvT {-proxy-} m' p  m a)
            -> GuiEnvT {-proxy-} m' p  m a
withGuiComponents f = getEnv >>= f . guiComponents

{-
withNewSolutionDialog :: (Monad m)
                      => (NewSolutionDialog -> GuiEnvT {-proxy-} m' p m a)
                      -> GuiEnvT {-proxy-} m' p m a
withNewSolutionDialog f = getEnv >>= f . newSolutionDialog
-}
newtype GuiEnvT {-proxy-} (m' :: * -> *) p m a 
    = GuiEnvT { runGuiEnvTInternal :: ReaderT (GuiEnv {-proxy-} m' p) m a }
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

instance (GuiViewerClass m) => GuiViewerClass (GuiEnvT {-proxy-} m' p m) where
    setSearchMode = lift . setSearchMode
    getSearchMode = lift getSearchMode
    openDeclaration = lift . openDeclaration
    closeDeclaration = lift . closeDeclaration
    getOpenDeclarations = lift getOpenDeclarations
    declarationIsOpen = lift . declarationIsOpen
    openDeclarationInHistory = lift .-.. openDeclarationInHistory
    replaceHistoryPath = lift . replaceHistoryPath
    replaceHistoryText = lift . replaceHistoryText
    navigateHistoryBack = lift navigateHistoryBack
    navigateHistoryForward = lift navigateHistoryForward
    updateHistoryPath = lift .-.. updateHistoryPath

runGuiEnvT :: GuiEnvT {-proxy-} m' p  m a -> GuiEnv {-proxy-} m' p  -> m a
runGuiEnvT = runReaderT . runGuiEnvTInternal

mkGuiEnvT :: (GuiEnv {-proxy-} m' p  -> m a) -> GuiEnvT {-proxy-} m' p  m a
mkGuiEnvT = GuiEnvT . ReaderT

getEnv :: (Monad m) => GuiEnvT {-proxy-} m' p  m (GuiEnv {-proxy-} m' p )
getEnv = GuiEnvT $ ask

mapGuiEnv :: (m1 a -> m2 b) 
          -> GuiEnvT {-proxy-} m' p  m1 a 
          -> GuiEnvT {-proxy-} m' p  m2 b
mapGuiEnv f m = GuiEnvT $ mapReaderT f $ runGuiEnvTInternal m 

{-
type GuiEnvSignal proxy m' p  m gui object m'' a
    = GuiEnvT {-proxy-} m' p  m (GuiSignal2 gui object (GuiEnvT {-proxy-} m' p  m'' a) (m'' a))

type GuiEnvSignal2 proxy m' p  m gui object f m'' a
    = GuiEnvT {-proxy-} m' p  m (GuiSignal2 gui object (f (GuiEnvT {-proxy-} m' p  m'' a)) (f (m'' a)))

mkGuiEnvSignal :: (Monad m, MonadIO m'')
               => (gui -> object) 
               -> Signal object (m'' a)
               -> GuiEnvSignal proxy m' p  m gui object m'' a
mkGuiEnvSignal getter sig = do
    env <- getEnv
    return $ getter `mkGuiSignalWith` sig $ flip runGuiEnvT env

wrapGuiEnvSignal :: (Monad m, MonadIO m'')
                 => (gui -> object)
                 -> GuiEnvSignal proxy m' p  m object subobject m'' a
                 -> GuiEnvSignal proxy m' p  m gui subobject m'' a
wrapGuiEnvSignal getter sig = liftM (wrapGuiSignal getter) sig
    

mkGuiEnvSignal2 :: (Monad m, MonadIO m'', Functor f)
               => (gui -> object) 
               -> Signal object (f (m'' a))
               -> GuiEnvSignal2 proxy m' p  m gui object f m'' a
mkGuiEnvSignal2 getter sig = do
    env <- getEnv
    --let x = (fmap (runGuiEnvT env) :: _)
    --let modifier = \action -> withReader fmap $ runGuiEnvT action env
    return $ getter `mkGuiSignalWith` sig $ fmap (flip runGuiEnvT env)


{-

f (t m a) -> f (m a)

-}

mkGuiEnvSignalFor :: (Monad m, MonadIO m'')
                 => (gui -> object)
                 -> (object -> subobject)
                 -> Signal subobject (m'' a)
                 -> GuiEnvSignal proxy m' p  m gui subobject m'' a
mkGuiEnvSignalFor obj subobj event = (subobj . obj) `mkGuiEnvSignal` event

mkGuiEnvSignal2For :: (Monad m, MonadIO m'', Functor f)
                 => (gui -> object)
                 -> (object -> subobject)
                 -> Signal subobject (f (m'' a))
                 -> GuiEnvSignal2 proxy m' p  m gui subobject f m'' a
mkGuiEnvSignal2For obj subobj event = (subobj . obj) `mkGuiEnvSignal2` event
-}
