{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Graphics.UI.Gtk hiding (get)

import Ide3.Types
import Ide3.Monad
import Ide3.Digest
import Ide3.Mechanism.State
import qualified Ide3.Project as Project

import Viewer
import ViewerMonad
import ViewerMonad2

import ProjectTree

import GuiMonad
import GuiCommand
import GuiEnv

import GuiHelpers

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewProjectDialog (NewProjectDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
import ProjectContextMenu (ContextMenu)

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewProjectDialog as NewProjectDialog
import qualified Dialogs.NewModuleDialog as NewModuleDialog
import qualified ProjectContextMenu

import PseudoState

--import ReadOnlyFilesystemProject
--import SimpleFilesystemProject
import CabalFilesystemProject

import Initializer


--deriving instance (InteruptMonad2 x m) => InteruptMonad2 (FileSystemProject, x) (SimpleFilesystemProjectT' m)
--deriving instance (InteruptMonad2 x m) => InteruptMonad2 (FileSystemProject, x) (SimpleFilesystemProjectT m)
--deriving instance (InteruptMonad2 x m) => InteruptMonad2 (Project, x) (ProjectStateT m)
--deriving instance (InteruptMonad2 x m) => InteruptMonad2 (FileSystemProject, x) (StatefulProject m)
--deriving instance (InteruptMonad0 m) => InteruptMonad2 FileSystemProject (SimpleFilesystemProjectT' m)
--deriving instance (InteruptMonad0 m) => InteruptMonad2 FileSystemProject (SimpleFilesystemProjectT m)
--deriving instance (InteruptMonad0 m) => InteruptMonad2 Project (ProjectStateT m)
--deriving instance (InteruptMonad0 m) => InteruptMonad0 (StatefulProject m)

{-
instance InteruptMonad0 m => InteruptMonad1 (MVar FileSystemProject) (SimpleFilesystemProjectT m) where
    interupt1 var f = do
        s <- takeMVar var
        (x,s') <- interupt0 $ runSimpleFilesystemProjectT f s
        putMVar var s'
        return x

instance InteruptMonad2 s m => InteruptMonad2 (FileSystemProject,s) (SimpleFilesystemProjectT m) where
    interupt2 (s,s2) f = do
        ((x,s'),s2') <- interupt2 s2 $ runSimpleFilesystemProjectT f s
        return (x,(s',s2'))

instance InteruptMonad0 m => InteruptMonad2 Project (ProjectStateT m) where
    interupt2 s f = interupt0 $ runProjectStateT f s
-}


onNewProjectConfirmed :: forall proxy m p buffer
                       . ( MonadIO m
                         , ViewerMonad m
                         , InteruptMonad2 p m
                         , TextBufferClass buffer
                         , MonadMask m
                         )
                      => NewProjectDialog
                      -> GuiEnvT proxy m p buffer (EventM EButton) Bool
onNewProjectConfirmed dialog = do
    projectRoot <- liftIO $ NewProjectDialog.getSelectedFolder dialog
    projectName <- liftIO $ NewProjectDialog.getProjectName dialog
    templateName <- liftIO $ NewProjectDialog.getTemplateName dialog
    mapGuiEnv liftIO $ doNew projectRoot projectName templateName
    liftIO $ NewProjectDialog.close dialog
    return False
    

onNewClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onNewClicked = do
    dialog <- NewProjectDialog.make $ \gui -> do
    {-
        gui `onGui` NewProjectDialog.confirmClicked $ onNewProjectConfirmed gui
        gui `onGui` NewProjectDialog.cancelClicked $ do
            liftIO $ NewProjectDialog.close gui -}
            return False
    return False


onOpenClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onOpenClicked = do
    dialog <- liftIO $ fileChooserDialogNew
        Nothing
        Nothing
        FileChooserActionOpen
        [ ("Close", ResponseReject)
        , ("Open" , ResponseAccept)
        ]
    r <- liftIO $ dialogRun dialog
    case r of
        ResponseAccept -> do
            Just path <- liftIO $ fileChooserGetFilename dialog
            liftIO $ widgetDestroy dialog
            mapGuiEnv liftIO $ doOpen path
            liftIO $ setCurrentDirectory $ takeDirectory path
        ResponseReject -> liftIO $ widgetDestroy dialog
        _ -> liftIO $ widgetDestroy dialog
    return False

onDigestClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onDigestClicked = do
    dialog <- liftIO $ fileChooserDialogNew
        Nothing
        Nothing
        FileChooserActionSelectFolder
        [ ("Close", ResponseReject)
        , ("Open" , ResponseAccept)
        ]
    r <- liftIO $ dialogRun dialog
    case r of
        ResponseAccept -> do
            Just path <- liftIO $ fileChooserGetFilename dialog
            liftIO $ widgetDestroy dialog
            mapGuiEnv liftIO $ doOpen path
            liftIO $ setCurrentDirectory path
        ResponseReject -> liftIO $ widgetDestroy dialog
    return False



onDeclClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => TreePath
              -> TreeViewColumn
              -> GuiEnvT proxy m p buffer IO ()
onDeclClicked = doGetDecl

onBuildClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onBuildClicked = do
    mapGuiEnv liftIO $ doBuild
    return False

onRunClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onRunClicked = do
    mapGuiEnv liftIO $ doRun
    return False


onSaveClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onSaveClicked = do
    mapGuiEnv liftIO $ doSave
    return False

onSaveProjectClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnvT proxy m p buffer (EventM EButton) Bool
onSaveProjectClicked = do
    mapGuiEnv liftIO $ doSaveProject Nothing
    return False

onNewModuleClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => Maybe String
              -> GuiEnvT proxy m p buffer (EventM EButton) Bool
onNewModuleClicked modName = do
    NewModuleDialog.make modName $ \dialog -> do
        dialog `onGuiM` NewModuleDialog.confirmClickedEvent $ do
            moduleName <- NewModuleDialog.getModuleName dialog
            case moduleName of
                "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter a module name" ""
                name -> do
                    mapGuiEnv liftIO $ doAddModule (ModuleInfo (Symbol moduleName))
                    NewModuleDialog.close dialog
            return False
        dialog `onGuiM` NewModuleDialog.cancelClickedEvent $ do
            NewModuleDialog.close dialog
            return False
    return False

setupModuleContextMenu :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 ) 
                 => ModuleInfo
                 -> GuiEnvT proxy m p buffer IO ContextMenu
setupModuleContextMenu mi = do
    menu <- ProjectContextMenu.makeModuleMenu mi
    menu `onGuiM` ProjectContextMenu.newSubModuleClickedEvent $ do
        onNewModuleClicked $ case mi of
            mi@(ModuleInfo (Symbol prefix)) -> (Just prefix)
            mi -> Nothing
    return menu


setupProjectContextMenu :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 ) 
                 => GuiEnvT proxy m p buffer IO ContextMenu
setupProjectContextMenu = do
    menu <- ProjectContextMenu.makeProjectMenu
    menu `onGuiM` ProjectContextMenu.newModuleClickedEvent $ do
        onNewModuleClicked Nothing
    return menu

onDeclViewClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => MainWindow
              -> GuiEnvT proxy m p buffer (EventM EButton) Bool
onDeclViewClicked gui = do
    button <- lift $ eventButton
    when (button == RightButton) $ do
        (x,y) <- lift $ eventCoordinates
        time <- lift $ eventTime
        let (x',y') = (round x, round y)
        pathClicked <- MainWindow.getProjectPathClicked (x',y') gui
        menu <- case pathClicked of
            Nothing -> mapGuiEnv liftIO $ setupProjectContextMenu
            Just (path, col, p) -> withGuiComponents $ \comp -> do
                item <- withProjectTree comp $ liftIO . findAtPath path
                case item of
                    ModuleResult mi -> mapGuiEnv liftIO $ setupModuleContextMenu mi
                    DeclResult mi di -> ProjectContextMenu.makeDeclMenu mi di
                    ImportsResult mi -> ProjectContextMenu.makeImportsMenu mi
                    ExportsResult mi -> ProjectContextMenu.makeExportsMenu mi
                    ImportResult mi ii -> ProjectContextMenu.makeImportMenu mi ii
                    ExportResult mi ei -> ProjectContextMenu.makeExportMenu mi ei
                    NoSearchResult -> mapGuiEnv liftIO $ setupProjectContextMenu
        lift $ ProjectContextMenu.showMenu menu
    return False    

doMain :: forall proxy m p 
        . ( MonadIO m
          , ViewerMonad m
          , InteruptMonad2 p m
          , MonadMask m
          )
       => proxy m 
       -> p
       -> IO ()
doMain proxy init = do
    projectMVar <- newMVar (Viewer Nothing Nothing, init)
    components <- initializeComponents
    let env = GuiEnv proxy components projectMVar
    flip runGuiEnvT env $ MainWindow.make $ \gui -> do
        gui `onGuiM` MainWindow.newClickedEvent $ onNewClicked
        gui `onGuiM` MainWindow.openClickedEvent $ onOpenClicked
        gui `onGuiM` MainWindow.digestClickedEvent $ onDigestClicked
        gui `onGuiM` MainWindow.saveClickedEvent $ onSaveClicked
        gui `onGuiM` MainWindow.saveProjectClickedEvent $ onSaveProjectClicked
        gui `onGuiF` MainWindow.declClickedEvent $ Compose onDeclClicked
        gui `onGuiM` MainWindow.declViewClickedEvent $ onDeclViewClicked gui
        gui `onGuiM` MainWindow.buildClickedEvent $ onBuildClicked
        gui `onGuiM` MainWindow.runClickedEvent $ onRunClicked
        gui `onGuiM` MainWindow.windowClosedEvent $ do
            liftIO exitSuccess
            return False
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (CabalProject (ProjectStateT IO)))
              (Unopened, Project.empty)
