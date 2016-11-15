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

import Data.Tree
import Data.Proxy
import Data.Functor.Compose

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding (withState)

import Control.Concurrent.MVar


import GI.Gtk hiding (main, on)
import qualified GI.Gtk as Gtk
import GI.Gdk hiding (window, on)


import Ide3.Types
import Ide3.Utils
import Ide3.Types.State
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Strict
import Ide3.Digest
import qualified Ide3.Solution as Solution

import Viewer
import ViewerMonad
import ViewerMonad2

import SolutionTree

import GuiMonad
import GuiCommand
import GuiEnv

import GuiHelpers

import Dialogs
import Dialogs.Class

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
import Dialogs.NewImportDialog (NewImportDialog)
import Dialogs.NewExportDialog (NewExportDialog)
import SolutionContextMenu (ContextMenu)

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.NewProjectDialog as NewProjectDialog
import qualified Dialogs.NewModuleDialog as NewModuleDialog
import qualified Dialogs.NewImportDialog as NewImportDialog
import qualified Dialogs.NewExportDialog as NewExportDialog
import qualified SolutionContextMenu

import GuiT

import SearchMode

import PseudoState

import GuiViewer
import GuiViewer.Class

import CabalMonad
import CabalFilesystemSolution

import Initializer

import GuiClass
import GuiClass.GuiEnv
--import GuiClass.GuiEnv.Stack
import GuiClass.GuiT

import EnvironmentMonad
import EnvironmentMonad.Stack

import Args

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
    type ProjectArgType (ViewerStateT m) = ProjectArgType m

instance ProjectInitializerMonad m => ProjectInitializerMonad (GuiViewerT m) where
    getProjectInitializer = liftM (mapProjectInitializer lift) $ lift getProjectInitializer
    type ProjectArgType (GuiViewerT m) = ProjectArgType m

{-
doMain :: forall proxy m p 
        . ( MainGuiClass (GuiT m p) m p IO )
       => proxy m 
       -> p
       -> IO ()
-}
doMain :: a -> (FileSystemSolution,Solution) -> IO ()
doMain _ init = do
    projectMVar <- newMVar (emptyGuiViewer,(emptyViewer, init))
    _ <- Gtk.init Nothing
    components <- initializeComponents
    --manager <- uiManagerNew
    --group <- uiManagerGetAccelGroup manager
    group <- Gtk.new AccelGroup []
    
    let env = GuiEnv {-proxy-} components projectMVar
    dialogs <- flip runGuiEnvT env $ do
        withGuiComponents $ applyDeclBufferAttrs defaultTextAttrs
        newSolutionDialog <- NewSolutionDialog.make $ \dialog -> do
            runIdentityT $ dialog `on1` NewSolutionDialog.cancelClicked $ \event -> lift $ do
                NewSolutionDialog.setVisible dialog False
                return False
            return dialog
        newProjectDialog <- NewProjectDialog.make $ \dialog -> do
            runIdentityT $ do
                dialog `on1` NewProjectDialog.cancelClicked $ \event -> lift $ do
                    NewProjectDialog.setVisible dialog False
                    return False
                dialog `on` NewProjectDialog.projectTypeChanged $ do
                    newProjectType <- NewProjectDialog.getProjectType dialog
                    NewProjectDialog.setProjectType dialog newProjectType
                dialog `on` NewProjectDialog.testTypeChanged $ do
                    newTestType <- NewProjectDialog.getTestProjectType dialog
                    NewProjectDialog.setTestProjectType dialog newTestType
            return dialog
        mainWindow <- MainWindow.make $ \gui -> do
            setupKeyboardShortcuts gui group
            return gui
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
            newSolutionDialog `on1` NewSolutionDialog.confirmClicked $ \event -> do
                onNewSolutionConfirmed
                --GuiT (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))) (FileSystemSolution, Solution) IO ()
                return False
            newProjectDialog `on1` NewProjectDialog.confirmClicked $ \event -> do
                onNewProjectConfirmed
                return False
            Gtk.main
    
    runGuiT go env dialogs



main :: IO ()
main = doMain (Proxy :: Proxy (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))))
              (Unopened, Solution.empty)

