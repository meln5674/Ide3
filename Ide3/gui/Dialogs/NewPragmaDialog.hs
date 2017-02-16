{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module Dialogs.NewPragmaDialog 
    ( NewPragmaDialog
    , makeNew
    , makeEdit
    , close
    , NewPragmaDialogSignal
    , getPragma
    , setVisible
    , confirmClickedEvent
    , confirmPressedEvent
    , cancelClickedEvent
    ) where

import Data.Text

import Control.Monad.Trans


import GI.Gtk

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewPragmaDialog = NewPragmaDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewPragmaDialog where
    getGenericDialog = Dialogs.NewPragmaDialog.getGenericDialog

makeNew :: (MonadIO m) => (NewPragmaDialog -> m a) -> m a
makeNew = Generic.make NewPragmaDialog "New Pragma" Nothing

makeEdit :: (MonadIO m) => Text -> (NewPragmaDialog -> m a) -> m a
makeEdit existing = Generic.make NewPragmaDialog "Edit Pragma" (Just existing)

close :: (MonadIO m) => NewPragmaDialog -> m ()
close = Generic.close

type NewPragmaDialogSignal object info = GenericNewDialogSignal NewPragmaDialog object info

getPragma :: (MonadIO m) => NewPragmaDialog -> m Text
getPragma = Generic.getEnteredText

confirmClickedEvent :: NewPragmaDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClickedEvent = Generic.confirmClickedEvent

confirmPressedEvent :: NewPragmaDialogSignal Entry EntryActivateSignalInfo
confirmPressedEvent = Generic.confirmPressedEvent

cancelClickedEvent :: NewPragmaDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClickedEvent = Generic.cancelClickedEvent

setVisible :: (MonadIO m) => NewPragmaDialog -> Bool -> m ()
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
