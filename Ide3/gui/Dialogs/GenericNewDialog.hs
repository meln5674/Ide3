{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module Dialogs.GenericNewDialog 
    ( GenericNewDialog
    , NewDialog (..)
    , make
    , close
    , GenericNewDialogSignal
    , getEnteredText
    , confirmClickedEvent
    , cancelClickedEvent
    ) where

import Prelude hiding (length)

import Data.Text

import Control.Monad
import Control.Monad.Trans

import GI.Gtk
import GI.Gdk hiding (Window)

import GuiEnv
import GuiHelpers

class NewDialog dialog where
    getGenericDialog :: dialog -> GenericNewDialog
    

data GenericNewDialog
    = GenericNewDialog
    { window :: Window
    , textEntryBuffer :: EntryBuffer
    , textEntryBox :: Entry
    , confirmButton :: Button
    , cancelButton :: Button
    }
{-
makeVBoxWith :: (MonadIO m, ContainerClass self) => self -> (VBox -> m b) -> m b
makeVBoxWith window f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ window `containerAdd` vbox
    f vbox

makeHBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b
makeHBoxWith vbox f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ boxPackEnd vbox hbox PackNatural 0
    f hbox
-}

makeTextEntryBox :: (MonadIO m, IsBox self) => self -> EntryBuffer -> m Entry
makeTextEntryBox vbox buffer = liftIO $ do
    textEntryBox <- entryNewWithBuffer buffer
    boxPackStart vbox textEntryBox True True 0
    return textEntryBox

makeConfirmButton :: (MonadIO m, IsBox self) => self -> m Button
makeConfirmButton hbox = liftIO $ do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackStart hbox confirmButton True True 0
    return confirmButton

makeCancelButton :: (MonadIO m, IsBox self) => self -> m Button
makeCancelButton hbox = liftIO $ do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton True True 0
    return cancelButton

make :: (MonadIO m, NewDialog dialog) 
     => (GenericNewDialog -> dialog) 
     -> Text
     -> Maybe Text
     -> (dialog -> m a) 
     -> m a
make makeDialog title initial f = makeWindowWith
    $ \window ->  makeVBoxWith window 
    $ \vbox -> do
        set window [windowTitle := title]
        textEntryBuffer <- entryBufferNew initial $ fromIntegral $ maybe 0 length initial
        textEntryBox <- makeTextEntryBox vbox textEntryBuffer
        (confirmButton,cancelButton) <- makeHBoxWith vbox $ \hbox -> do
            confirmButton <- makeConfirmButton hbox
            cancelButton <- makeCancelButton hbox
            return (confirmButton,cancelButton)
        f $ makeDialog GenericNewDialog
          { window
          , textEntryBuffer
          , textEntryBox
          , confirmButton
          , cancelButton
          }

close :: (MonadIO m, NewDialog dialog) => dialog -> m ()
close = liftIO . widgetDestroy . window . getGenericDialog

getEnteredText :: (MonadIO m, NewDialog dialog) => dialog -> m Text
getEnteredText = liftIO . flip get entryBufferText . textEntryBuffer . getGenericDialog

{-
type GenericNewDialogSignal proxy m' p  m dialog object m'' a
    = GuiEnvSignal proxy m' p  m dialog object m'' a
-}

type GenericNewDialogSignal dialog subObject info = SubSignalProxy dialog subObject info

confirmClickedEvent :: (NewDialog dialog) => GenericNewDialogSignal dialog Button WidgetButtonPressEventSignalInfo
confirmClickedEvent dialog = (confirmButton $ getGenericDialog dialog, #buttonPressEvent)

cancelClickedEvent :: (NewDialog dialog) => GenericNewDialogSignal dialog Button WidgetButtonPressEventSignalInfo
cancelClickedEvent dialog = (cancelButton $ getGenericDialog dialog, #buttonPressEvent)

