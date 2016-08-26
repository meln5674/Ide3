{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
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

import System.Glib.UTFString

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.Gtk

import GuiEnv
import GuiHelpers

import Dialogs.GenericNewDialog ( GenericNewDialog, GenericNewDialogSignal, NewDialog )
import qualified Dialogs.GenericNewDialog as Generic

newtype NewImportDialog = NewImportDialog { getGenericDialog :: GenericNewDialog }

instance NewDialog NewImportDialog where
    getGenericDialog = Dialogs.NewImportDialog.getGenericDialog

makeNew :: (MonadIO m) => (NewImportDialog -> m a) -> m a
makeNew = Generic.make NewImportDialog "New Import" (Just "import ")

makeEdit :: (MonadIO m) => String -> (NewImportDialog -> m a) -> m a
makeEdit existing = Generic.make NewImportDialog "Edit Import" (Just existing)

close :: (MonadIO m) => NewImportDialog -> m ()
close = Generic.close

type NewImportDialogSignal proxy m' p  m object m'' a
    = GenericNewDialogSignal proxy m' p  m NewImportDialog object m'' a

getImport :: (MonadIO m) => NewImportDialog -> m String
getImport = Generic.getEnteredText

confirmClickedEvent :: (Monad m) => NewImportDialogSignal proxy m' p  m Button (EventM EButton) Bool
confirmClickedEvent = Generic.confirmClickedEvent

cancelClickedEvent :: (Monad m) => NewImportDialogSignal proxy m' p  m Button (EventM EButton) Bool
cancelClickedEvent = Generic.cancelClickedEvent
