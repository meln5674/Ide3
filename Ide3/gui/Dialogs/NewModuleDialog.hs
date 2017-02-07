{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module Dialogs.NewModuleDialog 
    ( NewModuleDialog
    , make
    , close
    , NewModuleDialogSignal
    , getModuleName
    , setVisible
    , confirmClickedEvent
    , cancelClickedEvent
    ) where

import Data.Text

import Control.Monad.Trans


import GI.Gtk

import GuiHelpers

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewModuleDialog = NewModuleDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewModuleDialog where
    getGenericDialog = Dialogs.NewModuleDialog.getGenericDialog

make :: (MonadIO m) => Maybe Text -> (NewModuleDialog -> m a) -> m a
make = Generic.make NewModuleDialog "New Module Name" 

close :: (MonadIO m) => NewModuleDialog -> m ()
close = Generic.close

type NewModuleDialogSignal object info = GenericNewDialogSignal NewModuleDialog object info

getModuleName :: (MonadIO m) => NewModuleDialog -> m Text
getModuleName = Generic.getEnteredText

confirmClickedEvent :: NewModuleDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClickedEvent = Generic.confirmClickedEvent

cancelClickedEvent :: NewModuleDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClickedEvent = Generic.cancelClickedEvent

setVisible :: (MonadIO m) => NewModuleDialog -> Bool -> m ()
setVisible = Generic.setVisible

{-
data NewModuleDialog
    = NewModuleDialog
    { window :: Window
    , moduleNameBuffer :: EntryBuffer
    , moduleNameBox :: Entry
    , confirmButton :: Button
    , cancelButton :: Button    
    }

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


makeModuleNameBox :: (MonadIO m, BoxClass self) => self -> EntryBuffer -> m Entry
makeModuleNameBox vbox  = liftIO $ do
    moduleNameBox <- entryNewWithBuffer buffer
    boxPackStart vbox moduleNameBox PackGrow 0
    return moduleNameBox

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

make :: (MonadIO m) => Maybe String -> (NewModuleDialog -> m a) -> m a
make name f = makeWindowWith
    $ \window ->  makeVBoxWith window 
    $ \vbox -> do
        moduleNameBuffer <- liftIO $ entryBufferNew name
        moduleNameBox <- makeModuleNameBox vbox moduleNameBuffer
        (confirmButton,cancelButton) <- makeHBoxWith vbox $ \hbox -> do
            confirmButton <- makeConfirmButton hbox
            cancelButton <- makeCancelButton hbox
            return (confirmButton,cancelButton)
        f NewModuleDialog
          { window
          , moduleNameBuffer
          , moduleNameBox
          , confirmButton
          , cancelButton
          }

close :: (MonadIO m) => NewModuleDialog -> m ()
close = liftIO . widgetDestroy . window

getModuleName :: (MonadIO m) => NewModuleDialog -> m String
getModuleName = liftIO . flip get entryBufferText . moduleNameBuffer

type NewModuleDialogSignal proxy m' p buffer m object m'' a
    = GuiEnvSignal proxy m' p buffer m NewModuleDialog object m'' a



confirmClickedEvent :: (Monad m) => NewModuleDialogSignal proxy m' p buffer m Button (EventM EButton) Bool
confirmClickedEvent = confirmButton `mkGuiEnvSignal` buttonPressEvent

cancelClickedEvent :: (Monad m) => NewModuleDialogSignal proxy m' p buffer m Button (EventM EButton) Bool
cancelClickedEvent = cancelButton `mkGuiEnvSignal` buttonPressEvent
-}
