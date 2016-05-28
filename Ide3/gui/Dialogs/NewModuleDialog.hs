{-# LANGUAGE NamedFieldPuns #-}
module Dialogs.NewModuleDialog 
    ( NewModuleDialog
    , make
    , close
    , NewModuleDialogSignal
    , getModuleName
    , confirmClickedEvent
    , cancelClickedEvent
    ) where

import System.Glib.UTFString

import Control.Monad

import Graphics.UI.Gtk

import GuiHelpers

data NewModuleDialog
    = NewModuleDialog
    { window :: Window
    , moduleNameBuffer :: EntryBuffer
    , moduleNameBox :: Entry
    , confirmButton :: Button
    , cancelButton :: Button    
    }

makeVBoxWith :: ContainerClass self => self -> (VBox -> IO b) -> IO b
makeVBoxWith window f = do
    vbox <- vBoxNew False 0
    window `containerAdd` vbox
    f vbox

makeHBoxWith :: BoxClass self => self -> (HBox -> IO b) -> IO b
makeHBoxWith vbox f = do
    hbox <- hBoxNew False 0
    boxPackEnd vbox hbox PackNatural 0
    f hbox


makeModuleNameBox :: BoxClass self => self -> EntryBuffer -> IO Entry
makeModuleNameBox vbox buffer = do
    moduleNameBox <- entryNewWithBuffer buffer
    boxPackStart vbox moduleNameBox PackGrow 0
    return moduleNameBox

makeConfirmButton :: BoxClass self => self -> IO Button
makeConfirmButton hbox = do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackStart hbox confirmButton PackGrow 0
    return confirmButton

makeCancelButton :: BoxClass self => self -> IO Button
makeCancelButton hbox = do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton PackGrow 0
    return cancelButton

make :: Maybe String -> (NewModuleDialog -> IO a) -> IO a
make name f = makeWindowWith
    $ \window ->  makeVBoxWith window 
    $ \vbox -> do
        moduleNameBuffer <- entryBufferNew name
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

close :: NewModuleDialog -> IO ()
close = widgetDestroy . window

getModuleName :: NewModuleDialog -> IO String
getModuleName = flip get entryBufferText . moduleNameBuffer

type NewModuleDialogSignal = GuiSignal NewModuleDialog



confirmClickedEvent :: NewModuleDialogSignal Button (EventM EButton Bool)
confirmClickedEvent = confirmButton `mkGuiSignal` buttonPressEvent

cancelClickedEvent :: NewModuleDialogSignal Button (EventM EButton Bool)
cancelClickedEvent = cancelButton `mkGuiSignal` buttonPressEvent

