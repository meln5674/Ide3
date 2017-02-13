{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Dialogs.EditSolutionDialog
    ( EditSolutionDialog
    , EditSolutionDialogSignal
    , make
    , close
    , setVisible
    , confirmClicked
    , cancelClicked
    , getConfigurationText
    , setConfigurationText
    ) where

import Data.Text (Text)

import Control.Monad.Trans

import GI.Gtk

import GuiHelpers

import BetterTextView

data EditSolutionDialog
    = EditSolutionDialog
    { window :: Window
    , editor :: BetterTextView
    , editorBuffer :: TextBuffer
    , confirmButton :: Button
    , cancelButton :: Button
    }


type EditSolutionDialogSignal subObject info = SubSignalProxy EditSolutionDialog subObject info

confirmClicked :: EditSolutionDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClicked dialog = (confirmButton dialog, #buttonPressEvent)

cancelClicked :: EditSolutionDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClicked dialog = (cancelButton dialog, #buttonPressEvent)

getConfigurationText :: (MonadIO m) => EditSolutionDialog -> m Text
getConfigurationText dialog = maybe "" id <$> get (editorBuffer dialog) #text

setConfigurationText :: (MonadIO m) => EditSolutionDialog -> Text -> m ()
setConfigurationText dialog text = set (editorBuffer dialog) [ #text := text ]



setVisible :: MonadIO m => EditSolutionDialog -> Bool -> m ()
setVisible dialog v = set (window dialog) [widgetVisible := v]

close :: MonadIO m => EditSolutionDialog -> m ()
close = liftIO . widgetDestroy . window

make :: (MonadIO m) => (EditSolutionDialog -> m b) -> m b
make f = makeWindowWith
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        editor <- makeScrolledWindowWith vbox $ \scrolledWindow -> do
            editor <- betterTextViewNew
            scrolledWindow `containerAdd` editor
            return editor
        editorBuffer <- getSub' editor #buffer
        makeHBoxWith vbox
            $ \hbox -> do
                boxSetChildPacking vbox hbox False False 0 PackTypeEnd
                confirmButton <- makeButton "Confirm" hbox
                cancelButton <- makeButton "Cancel" hbox
                boxSetChildPacking hbox confirmButton True True 0 PackTypeStart
                boxSetChildPacking hbox cancelButton True True 0 PackTypeEnd
                f EditSolutionDialog
                    { window
                    , editor
                    , editorBuffer
                    , confirmButton
                    , cancelButton
                    }
        
    
