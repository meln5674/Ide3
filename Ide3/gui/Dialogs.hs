{-|
Module      : BetterTextView
Description : ReaderT implementation of DialogsClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dialogs where

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Ide3.Utils

import Dialogs.Class

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewProjectDialog (NewProjectDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
--import Dialogs.NewPragmaDialog (NewPragmaDialog)
import Dialogs.NewExportDialog (NewExportDialog)
import Dialogs.NewImportDialog (NewImportDialog)

data Dialogs = Dialogs
    { mainWindow :: MainWindow
    , newSolutionDialog :: NewSolutionDialog
    , newProjectDialog :: NewProjectDialog
    , newModuleDialog :: NewModuleDialog
    -- , newPragmaDialog :: NewPragmaDialog
    , newExportDialog :: NewExportDialog
    , newImportDialog :: NewImportDialog
    }

newtype DialogsT m a = DialogsT { runDialogsTInternal :: ReaderT Dialogs m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadBounce
    )

runDialogsT :: DialogsT m a -> Dialogs -> m a
runDialogsT f dialogs = runReaderT (runDialogsTInternal f) dialogs

mkDialogsT :: (Dialogs -> m a) -> DialogsT m a
mkDialogsT = DialogsT . ReaderT

getDialogs :: ( Monad m ) => DialogsT m Dialogs
getDialogs = DialogsT ask

instance (Monad m) => DialogsClass (DialogsT m) where
    withMainWindow f = DialogsT $ asks (f . mainWindow)
    withNewSolutionDialog f = DialogsT $ asks (f . newSolutionDialog)
    withNewProjectDialog f = DialogsT $ asks (f . newProjectDialog)
    withNewModuleDialog = undefined
    withNewExportDialog = undefined
    withNewImportDialog = undefined
