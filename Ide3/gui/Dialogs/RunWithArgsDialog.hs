{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Dialogs.RunWithArgsDialog
    ( RunWithArgsDialog
    , make
    , close
    , getArguments
    , confirmClicked
    , cancelClicked
    , setVisible
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans

import GI.Gtk

import GuiHelpers

import BetterTextView

data RunWithArgsDialog
    = RunWithArgsDialog
    { window :: Window
    , editor :: BetterTextView
    , editorBuffer :: TextBuffer
    , confirmButton :: Button
    , cancelButton :: Button
    }


type RunWithArgsDialogSignal subObject info = SubSignalProxy RunWithArgsDialog subObject info

confirmClicked :: RunWithArgsDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClicked dialog = (confirmButton dialog, #buttonPressEvent)

cancelClicked :: RunWithArgsDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClicked dialog = (cancelButton dialog, #buttonPressEvent)

getArguments :: (MonadIO m) => RunWithArgsDialog -> m [String]
getArguments dialog = maybe [] (map T.unpack . T.lines) <$> get (editorBuffer dialog) #text

setVisible :: MonadIO m => RunWithArgsDialog -> Bool -> m ()
setVisible dialog v = set (window dialog) [widgetVisible := v]

close :: MonadIO m => RunWithArgsDialog -> m ()
close = liftIO . widgetDestroy . window

make :: (MonadIO m) => (RunWithArgsDialog -> m b) -> m b
make f = makeWindowWith
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        makeLabel "Enter commands on separate lines" vbox
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
                f RunWithArgsDialog
                    { window
                    , editor
                    , editorBuffer
                    , confirmButton
                    , cancelButton
                    }
        
    
