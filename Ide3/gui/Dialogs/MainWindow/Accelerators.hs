module Dialogs.MainWindow.Accelerators where

import Data.Text (Text)

import Control.Monad.Trans

import GI.Gdk
import GI.Gtk

import GuiHelpers

import Dialogs.MainWindow.Types

addAccelGroup :: (MonadIO m) => MainWindow -> AccelGroup -> m ()
addAccelGroup w g = liftIO $ window w `windowAddAccelGroup` g

addMainWindowAccelerator :: ( MonadIO m
                          , IsAccelGroup group
                          , Integral key
                          , IsWidget subObject
                          )
                       => (MainWindow -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addMainWindowAccelerator f e = f `addAccel` e
