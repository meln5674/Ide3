{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GuiEnv where

import Data.Functor.Identity

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Graphics.UI.Gtk

import GuiHelpers
import Viewer
import GuiMonad

data GuiEnv proxy m p buffer
    = GuiEnv
    { proxy :: proxy m
    , guiComponents :: GuiComponents buffer
    , projectMVar :: MVar (ViewerState, p)
    }

withSolutionMVar :: (TextBufferClass buffer, Monad m)
                => (MVar (ViewerState,p) -> GuiEnvT proxy m' p buffer m a)
                -> GuiEnvT proxy m' p buffer m a
withSolutionMVar f = getEnv >>= f . projectMVar 

withGuiComponents :: (TextBufferClass buffer, Monad m)
            => (GuiComponents buffer -> GuiEnvT proxy m' p buffer m a)
            -> GuiEnvT proxy m' p buffer m a
withGuiComponents f = getEnv >>= f . guiComponents


newtype GuiEnvT proxy m' p buffer m a 
    = GuiEnvT { runGuiEnvTInternal :: ReaderT (GuiEnv proxy m' p buffer) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    )

runGuiEnvT :: (Monad m) => GuiEnvT proxy m' p buffer m a -> GuiEnv proxy m' p buffer -> m a
runGuiEnvT = runReaderT . runGuiEnvTInternal

mkGuiEnvT :: (Monad m) => (GuiEnv proxy m' p buffer -> m a) -> GuiEnvT proxy m' p buffer m a
mkGuiEnvT = GuiEnvT . ReaderT

getEnv :: (Monad m) => GuiEnvT proxy m' p buffer m (GuiEnv proxy m' p buffer)
getEnv = GuiEnvT $ ask

mapGuiEnv :: (Monad m1, Monad m2) 
          => (m1 a -> m2 b) 
          -> GuiEnvT proxy m' p buffer m1 a 
          -> GuiEnvT proxy m' p buffer m2 b
mapGuiEnv f m = GuiEnvT $ mapReaderT f $ runGuiEnvTInternal m 


type GuiEnvSignal proxy m' p buffer m gui object m'' a
    = GuiEnvT proxy m' p buffer m (GuiSignal2 gui object (GuiEnvT proxy m' p buffer m'' a) (m'' a))

type GuiEnvSignal2 proxy m' p buffer m gui object f m'' a
    = GuiEnvT proxy m' p buffer m (GuiSignal2 gui object (f (GuiEnvT proxy m' p buffer m'' a)) (f (m'' a)))

mkGuiEnvSignal :: (Monad m, MonadIO m'')
               => (gui -> object) 
               -> Signal object (m'' a)
               -> GuiEnvSignal proxy m' p buffer m gui object m'' a
mkGuiEnvSignal getter sig = do
    env <- getEnv
    return $ getter `mkGuiSignalWith` sig $ flip runGuiEnvT env

mkGuiEnvSignal2 :: (Monad m, MonadIO m'', Functor f)
               => (gui -> object) 
               -> Signal object (f (m'' a))
               -> GuiEnvSignal2 proxy m' p buffer m gui object f m'' a
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
                 -> GuiEnvSignal proxy m' p buffer m gui subobject m'' a
mkGuiEnvSignalFor obj subobj event = (subobj . obj) `mkGuiEnvSignal` event

mkGuiEnvSignal2For :: (Monad m, MonadIO m'', Functor f)
                 => (gui -> object)
                 -> (object -> subobject)
                 -> Signal subobject (f (m'' a))
                 -> GuiEnvSignal2 proxy m' p buffer m gui subobject f m'' a
mkGuiEnvSignal2For obj subobj event = (subobj . obj) `mkGuiEnvSignal2` event
