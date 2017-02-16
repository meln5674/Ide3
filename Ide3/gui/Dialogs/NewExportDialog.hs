{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Dialogs.NewExportDialog 
    ( NewExportDialog
    , makeNew
    , makeEdit
    , close
    , NewExportDialogSignal
    , getExport
    , confirmClickedEvent
    , confirmPressedEvent
    , cancelClickedEvent
    ) where

import Data.Text

import Control.Monad.Trans

import GI.Gtk

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewExportDialog = NewExportDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewExportDialog where
    getGenericDialog = Dialogs.NewExportDialog.getGenericDialog

makeNew :: (MonadIO m) => (NewExportDialog -> m a) -> m a
makeNew = Generic.make NewExportDialog "New Export" Nothing

makeEdit :: (MonadIO m) => Text -> (NewExportDialog -> m a) -> m a
makeEdit existing = Generic.make NewExportDialog "Edit Export" (Just existing)

close :: (MonadIO m) => NewExportDialog -> m ()
close = Generic.close

type NewExportDialogSignal object info = GenericNewDialogSignal NewExportDialog object info

getExport :: (MonadIO m) => NewExportDialog -> m Text
getExport = Generic.getEnteredText

confirmClickedEvent :: NewExportDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClickedEvent = Generic.confirmClickedEvent

confirmPressedEvent :: NewExportDialogSignal Entry EntryActivateSignalInfo
confirmPressedEvent = Generic.confirmPressedEvent

cancelClickedEvent :: NewExportDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClickedEvent = Generic.cancelClickedEvent
