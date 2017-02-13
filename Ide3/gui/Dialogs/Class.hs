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

{-# LANGUAGE TypeFamilies #-}
module Dialogs.Class where

import Control.Monad

import Dialogs.MainWindow (MainWindow)
{-
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewProjectDialog (NewProjectDialog)
-}

class Monad m => DialogsClass m where
    type NewSolutionDialog m
    type NewProjectDialog m
    withMainWindow :: (MainWindow -> a) -> m a
    withNewSolutionDialog :: (NewSolutionDialog m -> a) -> m a
    withNewProjectDialog :: (NewProjectDialog m -> a) -> m a

withMainWindowM :: DialogsClass m => (MainWindow -> m a) -> m a
withMainWindowM = join . withMainWindow

withNewSolutionDialogM :: DialogsClass m => (NewSolutionDialog m -> m a) -> m a
withNewSolutionDialogM = join . withNewSolutionDialog

withNewProjectDialogM :: DialogsClass m => (NewProjectDialog m -> m a) -> m a
withNewProjectDialogM = join . withNewProjectDialog
