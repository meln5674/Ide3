{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State.Strict hiding (withState)

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan

import GI.Gtk hiding (main, on)
import qualified GI.Gtk as Gtk
import GI.Gdk hiding (on)


import Ide3.Types
import Ide3.Types.State
import Ide3.NewMonad.Instances.Lift
import Ide3.NewMonad.Instances.Lift.TH
import Ide3.NewMonad.Instances.State
import Ide3.NewMonad.Instances.State.Class.Instances.Strict
import qualified Ide3.Solution as Solution

import Viewer
import ViewerMonad2

import GuiMonad
import GuiEnv

import GuiHelpers

import Dialogs
import Dialogs.Class

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.EditSolutionDialog as EditSolutionDialog
import qualified Dialogs.NewProjectDialog as NewProjectDialog

import GuiT

import PseudoState

import GuiViewer

import CabalFilesystemSolution

import Initializer

import GuiClass
import GuiClass.GuiEnv
import GuiClass.GuiT()

import EnvironmentMonad
import EnvironmentMonad.Stack()

import MainSignals

deriving instance (MonadMask m) => MonadMask (StatefulWrapper m)
deriving instance (MonadCatch m) => MonadCatch (StatefulWrapper m)
deriving instance (MonadThrow m) => MonadThrow (StatefulWrapper m)

deriving instance (MonadMask m) => MonadMask (SolutionStateT m)
deriving instance (MonadCatch m) => MonadCatch (SolutionStateT m)
deriving instance (MonadThrow m) => MonadThrow (SolutionStateT m)

instance (InteruptMonad2 s m) => InteruptMonad2 s (StatefulWrapper m) where
    interupt2 x f = interupt2 x (runStatefulWrapper f)

instance PseudoStateT SolutionStateT Solution where
    runPseudoStateT = runStateT . runSolutionStateT

deriving instance MonadMask GtkIO
deriving instance MonadCatch GtkIO
deriving instance MonadThrow GtkIO
deriving instance InteruptMonad0 GtkIO

instance ErrorClass (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))) where
    displayError msg = lift $ lift $ lift $ lift $ lift $ displayError msg

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
ViewerStateT;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

[betterderiving|
id;
;
  BuilderMonad
, RunnerMonad
, InitializerMonad
, SolutionEditorMonad
, ProjectInitializerMonad;
GuiViewerT;
  getBuilder = liftBuilder
, getRunner = liftRunner
, getInitializer = liftInitializer
, getSolutionRetriever = liftSolutionRetriever
, getSolutionEditor = liftSolutionEditor
, getProjectInitializer = liftProjectInitializer
, getProjectEditor = liftProjectEditor
, getProjectRetriever =  liftProjectRetriever
, getProjectRemover = liftProjectRemover
|]

type MonadStack = GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))
type GuiState = (FileSystemSolution, Solution) 

makeDialogs :: AccelGroup -> GuiEnv m' p -> IO Dialogs
makeDialogs group = runGuiEnvT $ do
    withGuiComponents $ applyDeclBufferAttrs defaultTextAttrs
    newSolutionDialog <- NewSolutionDialog.make $ \dialog -> do
        void $ runIdentityT $
            dialog `on` NewSolutionDialog.cancelClicked $ Func1 $ \_ -> lift $ do
                NewSolutionDialog.setVisible dialog False
                return False
        return dialog
    newProjectDialog <- NewProjectDialog.make $ \dialog -> do
        runIdentityT $ void $
            dialog `on` NewProjectDialog.cancelClicked $ Func1 $ \_ -> lift $ do
                NewProjectDialog.setVisible dialog False
                return False
        return dialog
    editSolutionDialog <- EditSolutionDialog.make $ \dialog -> do
        void $ runIdentityT $
            dialog `on` EditSolutionDialog.cancelClicked $ Func1 $ \_ -> lift $ do
                EditSolutionDialog.setVisible dialog False
                return False
        return dialog
    mainWindow <- MainWindow.make $ \gui -> do
        setupKeyboardShortcuts gui group
        return gui
    newSolutionDialog `NewSolutionDialog.setVisible` False
    newProjectDialog `NewProjectDialog.setVisible` False
    editSolutionDialog `EditSolutionDialog.setVisible` False
    return Dialogs
        { mainWindow
        , newSolutionDialog
        , newProjectDialog
        , editSolutionDialog
        }

setupNewSolutionDialogSignals :: NewSolutionDialog.NewSolutionDialog -> GuiT MonadStack GuiState IO ()
setupNewSolutionDialogSignals newSolutionDialog = void $ 
    newSolutionDialog `on` NewSolutionDialog.confirmClicked $ Func1 $ \_ -> do
        onNewSolutionConfirmed
        return False

setupNewProjectDialogSignals :: NewProjectDialog.NewProjectDialog -> GuiT MonadStack GuiState IO ()
setupNewProjectDialogSignals newProjectDialog = void $
    newProjectDialog `on` NewProjectDialog.confirmClicked $ Func1 $ \_ -> do
        mode <- NewProjectDialog.getDialogMode newProjectDialog
        case mode of
            NewProjectDialog.CreateProject -> onNewProjectConfirmed
            NewProjectDialog.EditProject name 
                -> onEditProjectConfirmed $ ProjectInfo name
        return False

doMain :: GuiState -> IO ()
doMain initialState = do
    projectMVar <- newMVar (emptyGuiViewer,(emptyViewer, initialState))
    _ <- Gtk.init Nothing
    components <- initializeComponents
    group <- Gtk.new AccelGroup []
    idleQueue <- newTChanIO
    let env = GuiEnv components projectMVar idleQueue
    dialogs <- makeDialogs group env
    let go :: GuiT MonadStack GuiState IO ()
        go = do
            withMainWindowM setupMainSignals
            withNewSolutionDialogM setupNewSolutionDialogSignals
            withNewProjectDialogM setupNewProjectDialogSignals
            void $ GuiT $ intercept (GI.Gdk.threadsAddIdle 200 . ($ ())) 
                                    (const $ runIdleThread >> return True)
            Gtk.main
    
    runGuiT go env dialogs



main :: IO ()
main = doMain (Unopened, Solution.empty)

