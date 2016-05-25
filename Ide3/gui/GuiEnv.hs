{-# LANGUAGE PolyKinds #-}
module GuiEnv where

import Control.Concurrent

import Graphics.UI.Gtk


import Viewer
import GuiMonad

data GuiEnv proxy m p buffer
    = GuiEnv
    { proxy :: proxy m
    , guiComponents :: GuiComponents buffer
    , projectMVar :: MVar (ViewerState, p)
    }

withProjectMVar :: (TextBufferClass buffer)
                => GuiEnv proxy m p buffer
                -> (MVar (ViewerState,p) -> a)
                -> a
withProjectMVar env f = f $ projectMVar env

withGuiComponents :: (TextBufferClass buffer)
            => GuiEnv proxy m p buffer
            -> (GuiComponents buffer -> a)
            -> a
withGuiComponents env f = f $ guiComponents env
