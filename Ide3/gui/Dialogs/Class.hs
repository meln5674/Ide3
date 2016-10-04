{-|
Module      : Dialogs.Class
Description : Typeclass for monads which can perform actions on the
                application's dialogs
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Dialogs.Class where

import Control.Monad

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
--import Dialogs.NewProjectDialog (NewProjectDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
--import Dialogs.NewPragmaDialog (NewPragmaDialog)
import Dialogs.NewExportDialog (NewExportDialog)
import Dialogs.NewImportDialog (NewImportDialog)

class Monad m => DialogsClass m where
    withMainWindow :: (MainWindow -> a) -> m a
    withNewSolutionDialog :: (NewSolutionDialog -> a) -> m a
    --withNewProjectDialog :: (NewProjectDialog -> m a) -> m a
    withNewModuleDialog :: (NewModuleDialog -> a) -> m a
    --withNewPragmaDialog :: (NewPragmaDialog -> a) -> m a
    withNewExportDialog :: (NewExportDialog -> a) -> m a
    withNewImportDialog :: (NewImportDialog -> a) -> m a

withMainWindowM :: DialogsClass m => (MainWindow -> m a) -> m a
withMainWindowM = join . withMainWindow

withNewSolutionDialogM :: DialogsClass m => (NewSolutionDialog -> m a) -> m a
withNewSolutionDialogM = join . withNewSolutionDialog

--withNewProjectDialogM :: DialogsClass m => (NewProjectDialog -> m a) -> m a
--withNewProjectDialogM f = join . withNewProjectDialog

withNewModuleDialogM :: DialogsClass m => (NewModuleDialog -> m a) -> m a
withNewModuleDialogM = join . withNewModuleDialog

--withNewPragmaDialogM :: DialogsClass m => (NewPragmaDialog -> m a) -> m a
--withNewPragmaDialogM f = join . withNewPragmaDialog

withNewExportDialogM :: DialogsClass m => (NewExportDialog -> m a) -> m a
withNewExportDialogM = join . withNewExportDialog

withNewImportDialogM :: DialogsClass m => (NewImportDialog -> m a) -> m a
withNewImportDialogM = join . withNewImportDialog
