{-|
Module      : Dialogs.GenericNewDialog
Description : A dialog which contains a text box, and buttons for confirm/cancel
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

Several dialogs in this application are just a text box for user input, along
with a button to confirm, and a button to cancel. This module provies a type
and methods for quickly creating variations on this base.

-}

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

-- | Class of dialogs which are based on the generic one
class NewDialog dialog where
    -- | Retrieve the underlying generic dialog value
    getGenericDialog :: dialog -> GenericNewDialog
    
-- | ADT for the generic dialog
data GenericNewDialog
    = GenericNewDialog
    { window :: Window
    , textEntryBuffer :: EntryBuffer
    , textEntryBox :: Entry
    , confirmButton :: Button
    , cancelButton :: Button
    }

-- | Make and add the text entry
makeTextEntryBox :: (MonadIO m, IsBox self) => self -> EntryBuffer -> m Entry
makeTextEntryBox vbox buffer = liftIO $ do
    textEntryBox <- entryNewWithBuffer buffer
    boxPackStart vbox textEntryBox True True 0
    return textEntryBox

-- | Make and add the confirm button
makeConfirmButton :: (MonadIO m, IsBox self) => self -> m Button
makeConfirmButton hbox = liftIO $ do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackStart hbox confirmButton True True 0
    return confirmButton

-- | Make and add the cancel button
makeCancelButton :: (MonadIO m, IsBox self) => self -> m Button
makeCancelButton hbox = liftIO $ do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton True True 0
    return cancelButton

-- | Create the dialog and perform an action with it
make :: (MonadIO m, NewDialog dialog) 
     => (GenericNewDialog -> dialog) -- ^ Function to wrap the generic dialog in 
                                     -- the new type
     -> Text                         -- ^ Title Text
     -> Maybe Text                   -- ^ Initial input, if any
     -> (dialog -> m a)              -- ^ Action to perform with the wrapped dialog
     -> m a                          -- ^ Result of the action
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

-- | Close the dialog
close :: (MonadIO m, NewDialog dialog) => dialog -> m ()
close = widgetDestroy . window . getGenericDialog

-- | Get the input text from the dialog
getEnteredText :: (MonadIO m, NewDialog dialog) => dialog -> m Text
getEnteredText = flip get entryBufferText . textEntryBuffer . getGenericDialog

-- | Signals from the a wrapper dialog
type GenericNewDialogSignal dialog subObject info = SubSignalProxy dialog subObject info

-- | Signal sent when the confirm button is clicked
confirmClickedEvent :: (NewDialog dialog) => GenericNewDialogSignal dialog Button WidgetButtonPressEventSignalInfo
confirmClickedEvent dialog = (confirmButton $ getGenericDialog dialog, #buttonPressEvent)

-- | Signal sent when the cancel button is clicked
cancelClickedEvent :: (NewDialog dialog) => GenericNewDialogSignal dialog Button WidgetButtonPressEventSignalInfo
cancelClickedEvent dialog = (cancelButton $ getGenericDialog dialog, #buttonPressEvent)

