module Dialogs.MainWindow.Accelerators where

import Control.Monad.Trans

import GI.Gtk

import Dialogs.MainWindow.Types

addAccelGroup :: (MonadIO m) => MainWindow -> AccelGroup -> m ()
addAccelGroup w g = liftIO $ window w `windowAddAccelGroup` g

