{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Dialogs.NewImportDialog 
    ( NewImportDialog
    , makeNew
    , makeEdit
    , close
    , NewImportDialogSignal
    , getImport
    , confirmClickedEvent
    , cancelClickedEvent
    ) where

import Data.Text

import Control.Monad
import Control.Monad.Trans

import GI.Gtk
import GI.Gdk

import GuiEnv
import GuiHelpers

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewImportDialog = NewImportDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewImportDialog where
    getGenericDialog = Dialogs.NewImportDialog.getGenericDialog

makeNew :: (MonadIO m) => (NewImportDialog -> m a) -> m a
makeNew = Generic.make NewImportDialog "New Import" (Just "import ")

makeEdit :: (MonadIO m) => Text -> (NewImportDialog -> m a) -> m a
makeEdit existing = Generic.make NewImportDialog "Edit Import" (Just existing)

close :: (MonadIO m) => NewImportDialog -> m ()
close = Generic.close

type NewImportDialogSignal object info = GenericNewDialogSignal NewImportDialog object info

getImport :: (MonadIO m) => NewImportDialog -> m Text
getImport = Generic.getEnteredText

confirmClickedEvent :: NewImportDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClickedEvent = Generic.confirmClickedEvent

cancelClickedEvent :: NewImportDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClickedEvent = Generic.cancelClickedEvent
