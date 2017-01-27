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
module Main where

import qualified Data.Text as T

import Data.Proxy

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


--deriving instance InteruptMonad0 m => InteruptMonad0 (StackEnvironment m)
--deriving instance InteruptMonad1 p m => InteruptMonad1 p (StackEnvironment m)
--deriving instance InteruptMonad2 p m => InteruptMonad2 p (StackEnvironment m)


{-
deriving instance ViewerMonad m => ViewerMonad (StackEnvironment m)
deriving instance ViewerStateClass m => ViewerStateClass (StackEnvironment m)
deriving instance GuiViewerClass m => GuiViewerClass (StackEnvironment m)
deriving instance ErrorClass m => ErrorClass (StackEnvironment m)
-}


{-
instance CabalMonad m => CabalMonad (ViewerStateT m) where
    getCabalProjects = bounce getCabalProjects
    getCabalProject = bounce .-. getCabalProject
    getCabalProjectInfo = bounce .-. getCabalProjectInfo
    lookupCabalProject = bounce .-. lookupCabalProject
    addCabalProject = bounce .-.. addCabalProject
    updateCabalProject = bounce .-.. updateCabalProject
    removeCabalProject = bounce .-. removeCabalProject
    getPackageDescription = bounce getPackageDescription

instance CabalMonad m => CabalMonad (GuiViewerT m) where
    getCabalProjects = bounce getCabalProjects
    getCabalProject = bounce .-. getCabalProject
    getCabalProjectInfo = bounce .-. getCabalProjectInfo
    lookupCabalProject = bounce .-. lookupCabalProject
    addCabalProject = bounce .-.. addCabalProject
    updateCabalProject = bounce .-.. updateCabalProject
    removeCabalProject = bounce .-. removeCabalProject
    getPackageDescription = bounce getPackageDescription
-}


instance ErrorClass (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))) where
    displayError msg = lift $ lift $ lift $ lift $ lift $ displayError msg


instance BuilderMonad m => BuilderMonad (ViewerStateT m) where
    getBuilder = liftM (mapBuilder lift) $ lift getBuilder

instance BuilderMonad m => BuilderMonad (GuiViewerT m) where
    getBuilder = liftM (mapBuilder lift) $ lift getBuilder
    
instance RunnerMonad m => RunnerMonad (ViewerStateT m) where
    getRunner = liftM (mapRunner lift) $ lift getRunner

instance RunnerMonad m => RunnerMonad (GuiViewerT m) where
    getRunner = liftM (mapRunner lift) $ lift getRunner

instance InitializerMonad m => InitializerMonad (ViewerStateT m) where
    getInitializer = liftM (mapInitializer lift) $ lift getInitializer
    type ArgType (ViewerStateT m) = ArgType m

instance InitializerMonad m => InitializerMonad (GuiViewerT m) where
    getInitializer = liftM (mapInitializer lift) $ lift getInitializer
    type ArgType (GuiViewerT m) = ArgType m

instance ProjectInitializerMonad m => ProjectInitializerMonad (ViewerStateT m) where
    getProjectInitializer = liftM (mapProjectInitializer lift) $ lift getProjectInitializer
    getProjectEditor = liftM (mapProjectEditor lift) $ lift getProjectEditor
    getProjectRetriever = liftM (mapProjectRetriever lift) $ lift getProjectRetriever
    type ProjectArgType (ViewerStateT m) = ProjectArgType m

instance ProjectInitializerMonad m => ProjectInitializerMonad (GuiViewerT m) where
    getProjectInitializer = liftM (mapProjectInitializer lift) $ lift getProjectInitializer
    getProjectEditor = liftM (mapProjectEditor lift) $ lift getProjectEditor
    getProjectRetriever = liftM (mapProjectRetriever lift) $ lift getProjectRetriever
    type ProjectArgType (GuiViewerT m) = ProjectArgType m

{-
doMain :: forall proxy m p 
        . ( MainGuiClass (GuiT m p) m p IO )
       => proxy m 
       -> p
       -> IO ()
-}
doMain :: a -> (FileSystemSolution,Solution) -> IO ()
doMain _ initialState = do
    projectMVar <- newMVar (emptyGuiViewer,(emptyViewer, initialState))
    _ <- Gtk.init Nothing
    components <- initializeComponents
    --manager <- uiManagerNew
    --group <- uiManagerGetAccelGroup manager
    group <- Gtk.new AccelGroup []
    idleQueue <- newTChanIO
    let env = GuiEnv {-proxy-} components projectMVar idleQueue
    dialogs <- flip runGuiEnvT env $ do
        withGuiComponents $ applyDeclBufferAttrs defaultTextAttrs
        newSolutionDialog <- NewSolutionDialog.make $ \dialog -> do
            void $ runIdentityT $ dialog `on` NewSolutionDialog.cancelClicked $ Func1 $ \_ -> lift $ do
                NewSolutionDialog.setVisible dialog False
                return False
            return dialog
        newProjectDialog <- NewProjectDialog.make $ \dialog -> do
            runIdentityT $ do
                void $ dialog `on` NewProjectDialog.cancelClicked $ Func1 $ \_ -> lift $ do
                    NewProjectDialog.setVisible dialog False
                    return False
            return dialog
        mainWindow <- MainWindow.make $ \gui -> do
            setupKeyboardShortcuts gui group
            return gui
        --mainWindow `MainWindow.setSearchBarVisible` False
        newSolutionDialog `NewSolutionDialog.setVisible` False
        newProjectDialog `NewProjectDialog.setVisible` False
        return Dialogs
            { mainWindow
            , newSolutionDialog
            , newProjectDialog
            , newModuleDialog = undefined
            , newExportDialog = undefined
            , newImportDialog = undefined
            }
    let go :: GuiT (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))) (FileSystemSolution, Solution) IO ()
        go = do
            gui <- liftDialogs $ withMainWindow id
            newSolutionDialog <- liftDialogs $ withNewSolutionDialog id
            newProjectDialog <- liftDialogs $ withNewProjectDialog id
            setupSignals gui
            void $ newSolutionDialog `on` NewSolutionDialog.confirmClicked $ Func1 $ \_ -> do
                onNewSolutionConfirmed
                return False
            void $ newProjectDialog `on` NewProjectDialog.confirmClicked $ Func1 $ \_ -> do
                mode <- NewProjectDialog.getDialogMode newProjectDialog
                case mode of
                    NewProjectDialog.CreateProject -> onNewProjectConfirmed
                    NewProjectDialog.EditProject name 
                        -> onEditProjectConfirmed $ ProjectInfo name
                return False
            void $ GuiT $ intercept (GI.Gdk.threadsAddIdle 200 . ($ ())) 
                                    (const $ runIdleThread >> return True)
            Gtk.main
    
    runGuiT go env dialogs



main :: IO ()
main = doMain (Proxy :: Proxy (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))))
              (Unopened, Solution.empty)

