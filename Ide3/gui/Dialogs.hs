{-|
Module      : BetterTextView
Description : ReaderT implementation of DialogsClass
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dialogs where

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Ide3.Utils

import Dialogs.Class hiding (NewSolutionDialog, NewProjectDialog)
import qualified Dialogs.Class as D

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewProjectDialog (NewProjectDialog)

data Dialogs = Dialogs
    { mainWindow :: MainWindow
    , newSolutionDialog :: NewSolutionDialog
    , newProjectDialog :: NewProjectDialog
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
runDialogsT = runReaderT . runDialogsTInternal

mkDialogsT :: (Dialogs -> m a) -> DialogsT m a
mkDialogsT = DialogsT . ReaderT

getDialogs :: ( Monad m ) => DialogsT m Dialogs
getDialogs = DialogsT ask

instance (Monad m) => DialogsClass (DialogsT m) where
    type NewSolutionDialog (DialogsT m) = NewSolutionDialog
    type NewProjectDialog (DialogsT m) = NewProjectDialog
    withMainWindow f = DialogsT $ asks (f . mainWindow)
    withNewSolutionDialog f = DialogsT $ asks (f . newSolutionDialog)
    withNewProjectDialog f = DialogsT $ asks (f . newProjectDialog)
