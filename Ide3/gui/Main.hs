{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding (withState)

import Control.Concurrent.MVar

import Graphics.UI.Gtk hiding (get, TreePath)

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

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
import Dialogs.NewImportDialog (NewImportDialog)
import Dialogs.NewExportDialog (NewExportDialog)
import SolutionContextMenu (ContextMenu)

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.NewModuleDialog as NewModuleDialog
import qualified Dialogs.NewImportDialog as NewImportDialog
import qualified Dialogs.NewExportDialog as NewExportDialog
import qualified SolutionContextMenu

import SearchMode

import PseudoState

import GuiViewer
import GuiViewer.Class

import CabalMonad
import CabalFilesystemSolution

import Initializer

import GuiClass
import GuiClass.GuiEnv
import GuiClass.GuiEnv.Stack

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

setupKeyboardShortcuts gui group = liftIO $ do
    gui `MainWindow.addAccelGroup` group
    MainWindow.addNewClickedEventAccelerator gui group
        "n" [Control, Shift] [AccelVisible]
    MainWindow.addOpenClickedEventAccelerator gui group
        "o" [Control] [AccelVisible]
    MainWindow.addDigestClickedEventAccelerator gui group
        "o" [Control, Shift] [AccelVisible]
    MainWindow.addSaveClickedEventAccelerator gui group
        "s" [Control] [AccelVisible]
    MainWindow.addSaveSolutionClickedEventAccelerator gui group
        "s" [Control,Shift] [AccelVisible]
    MainWindow.addBuildClickedEventAccelerator gui group
        "F5" [] [AccelVisible]
    {-MainWindow.addFindClickedEventAccelerator gui group
        "f" [Control] [AccelVisible]-}
    {-MainWindow.addNavigateClickedEventAccelerator gui group
        "KP_Space" [Control] [AccelVisible]-}
    MainWindow.addGotoDeclarationEventAccelerator gui group
        "d" [Control] [AccelVisible]
    MainWindow.addBackEventAccelerator gui group
        "less" [Control] [AccelVisible]
    MainWindow.addForwardEventAccelerator gui group
        "greater" [Control] [AccelVisible]
    
doMain :: forall proxy m p 
        . ( MainGuiClass m p IO )
       => proxy m 
       -> p
       -> IO ()
doMain proxy init = do
    projectMVar <- newMVar (emptyGuiViewer,(emptyViewer, init))
    _ <- initGUI
    components <- initializeComponents
    manager <- uiManagerNew
    group <- uiManagerGetAccelGroup manager
    newSolutionDialog <- NewSolutionDialog.make $ \dialog -> do
        dialog `onGui` NewSolutionDialog.cancelClicked $ do
            NewSolutionDialog.setVisible False dialog
            return False
        return dialog
    let env = GuiEnv {-proxy-} components projectMVar () newSolutionDialog
    flip runGuiEnvT env $ do
        withGuiComponents $ liftIO . applyDeclBufferAttrs defaultTextAttrs
        liftIO $ newSolutionDialog `onGui` NewSolutionDialog.confirmClicked $ do
            liftIO $ runGuiEnvT onNewSolutionConfirmed env
            return False
        MainWindow.make $ \gui -> do
            setupSignals gui :: GuiEnvT m p IO ()
            setupKeyboardShortcuts gui group
        NewSolutionDialog.setVisible False newSolutionDialog
        liftIO mainGUI
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))))
              (Unopened, Solution.empty)

