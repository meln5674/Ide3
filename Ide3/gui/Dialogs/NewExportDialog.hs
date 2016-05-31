{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
module Dialogs.NewExportDialog 
    ( NewExportDialog
    , makeNew
    , makeEdit
    , close
    , NewExportDialogSignal
    , getExport
    , confirmClickedEvent
    , cancelClickedEvent
    ) where

import System.Glib.UTFString

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import GuiEnv
import GuiHelpers

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewExportDialog = NewExportDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewExportDialog where
    getGenericDialog = Dialogs.NewExportDialog.getGenericDialog

makeNew :: (MonadIO m) => (NewExportDialog -> m a) -> m a
makeNew = Generic.make NewExportDialog "New Export" Nothing

makeEdit :: (MonadIO m) => String -> (NewExportDialog -> m a) -> m a
makeEdit existing = Generic.make NewExportDialog "Edit Export" (Just existing)

close :: (MonadIO m) => NewExportDialog -> m ()
close = Generic.close

type NewExportDialogSignal proxy m' p buffer m object m'' a
    = GenericNewDialogSignal proxy m' p buffer m NewExportDialog object m'' a

getExport :: (MonadIO m) => NewExportDialog -> m String
getExport = Generic.getEnteredText

confirmClickedEvent :: (Monad m) => NewExportDialogSignal proxy m' p buffer m Button (EventM EButton) Bool
confirmClickedEvent = Generic.confirmClickedEvent

cancelClickedEvent :: (Monad m) => NewExportDialogSignal proxy m' p buffer m Button (EventM EButton) Bool
cancelClickedEvent = Generic.cancelClickedEvent
