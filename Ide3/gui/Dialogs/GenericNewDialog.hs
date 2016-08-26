{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
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

import System.Glib.UTFString

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk


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

makeTextEntryBox :: (MonadIO m, BoxClass self) => self -> EntryBuffer -> m Entry
makeTextEntryBox vbox buffer = liftIO $ do
    textEntryBox <- entryNewWithBuffer buffer
    boxPackStart vbox textEntryBox PackGrow 0
    return textEntryBox

makeConfirmButton :: (MonadIO m, BoxClass self) => self -> m Button
makeConfirmButton hbox = liftIO $ do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackStart hbox confirmButton PackGrow 0
    return confirmButton

makeCancelButton :: (MonadIO m, BoxClass self) => self -> m Button
makeCancelButton hbox = liftIO $ do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton PackGrow 0
    return cancelButton

make :: (MonadIO m, NewDialog dialog) => (GenericNewDialog -> dialog) -> String -> Maybe String -> (dialog -> m a) -> m a
make makeDialog title initial f = makeWindowWith
    $ \window ->  makeVBoxWith window 
    $ \vbox -> do
        liftIO $ window `set` [windowTitle := title]
        textEntryBuffer <- liftIO $ entryBufferNew initial
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

getEnteredText :: (MonadIO m, NewDialog dialog) => dialog -> m String
getEnteredText = liftIO . flip get entryBufferText . textEntryBuffer . getGenericDialog

type GenericNewDialogSignal proxy m' p  m dialog object m'' a
    = GuiEnvSignal proxy m' p  m dialog object m'' a



confirmClickedEvent :: (Monad m, NewDialog dialog) => GenericNewDialogSignal proxy m' p  m dialog Button (EventM EButton) Bool 
confirmClickedEvent = (confirmButton . getGenericDialog) `mkGuiEnvSignal` buttonPressEvent

cancelClickedEvent :: (Monad m, NewDialog dialog) => GenericNewDialogSignal proxy m' p  m dialog Button (EventM EButton) Bool
cancelClickedEvent = (cancelButton . getGenericDialog) `mkGuiEnvSignal` buttonPressEvent

