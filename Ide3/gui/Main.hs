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
                      => GuiEnv proxy m p buffer
                      -> NewProjectDialog
                      -> EventM EButton Bool
onNewProjectConfirmed env dialog = liftIO $ do
    putStrLn "confirmed"
    projectRoot <- NewProjectDialog.getSelectedFolder dialog
    projectName <- NewProjectDialog.getProjectName dialog
    templateName <- NewProjectDialog.getTemplateName dialog
    doNew env projectRoot projectName templateName
    NewProjectDialog.close dialog
    return False
    

onNewClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onNewClicked env = liftIO $ do
    dialog <- NewProjectDialog.make $ \gui -> do
        gui `onGui` NewProjectDialog.confirmClicked $ onNewProjectConfirmed env gui
        gui `onGui` NewProjectDialog.cancelClicked $ do
            liftIO $ NewProjectDialog.close gui
            return False
    return False

onOpenClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onOpenClicked env = liftIO $ do
    dialog <- fileChooserDialogNew
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
            doOpen env path
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
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onDigestClicked env = liftIO $ do
    dialog <- fileChooserDialogNew
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
            doOpen env path
            liftIO $ setCurrentDirectory path
        ResponseReject -> liftIO $ widgetDestroy dialog
    return False



onDeclClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => GuiEnv proxy m p buffer 
              -> TreePath
              -> TreeViewColumn
              -> IO ()
onDeclClicked = doGetDecl

onBuildClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onBuildClicked env = do
    liftIO $ doBuild env
    return False

onRunClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onRunClicked env = do
    liftIO $ doRun env
    return False


onSaveClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onSaveClicked env = do
    liftIO $ doSave env
    return False

onSaveProjectClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onSaveProjectClicked env = do
    liftIO $ doSaveProject env Nothing
    return False

onDeclViewClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 , MonadMask m
                 )
              => GuiEnv proxy m p buffer 
              -> MainWindow
              -> EventM EButton Bool
onDeclViewClicked env gui = do
    button <- eventButton
    when (button == RightButton) $ do
        (x,y) <- eventCoordinates
        time <- eventTime
        let (x',y') = (round x, round y)
        pathClicked <- liftIO $ MainWindow.getProjectPathClicked (x',y') gui
        menu <- liftIO $ case pathClicked of
            Nothing -> do 
                menu <- ProjectContextMenu.makeProjectMenu
                liftIO $ menu `onGui` ProjectContextMenu.newModuleClickedEvent $ liftIO $ do
                    NewModuleDialog.make Nothing $ \dialog -> liftIO $ do
                        dialog `onGui` NewModuleDialog.confirmClickedEvent $ liftIO $ do
                            moduleName <- NewModuleDialog.getModuleName dialog
                            case moduleName of
                                "" -> dialogOnError env () $ do
                                    throwE $ InvalidOperation "Please enter a module name" ""
                                name -> do
                                    doAddModule env (ModuleInfo (Symbol moduleName))
                                    NewModuleDialog.close dialog
                            return False
                        dialog `onGui` NewModuleDialog.cancelClickedEvent $ liftIO $ do
                            NewModuleDialog.close dialog
                            return False
                    return False
                return menu
            Just (path, col, p) -> withGuiComponents env $ \comp -> do
                item <- withProjectTree comp $ findAtPath path
                case item of
                    ModuleResult mi -> do
                        menu <- ProjectContextMenu.makeModuleMenu mi
                        return menu
                    DeclResult mi di -> ProjectContextMenu.makeDeclMenu mi di
                    ImportsResult mi -> ProjectContextMenu.makeImportsMenu mi
                    ExportsResult mi -> ProjectContextMenu.makeExportsMenu mi
                    ImportResult mi ii -> ProjectContextMenu.makeImportMenu mi ii
                    ExportResult mi ei -> ProjectContextMenu.makeExportMenu mi ei
                    NoSearchResult -> do
                        menu <- ProjectContextMenu.makeProjectMenu
                        return menu
        ProjectContextMenu.showMenu menu
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
    MainWindow.make env $ \gui -> do
        gui `onGui` MainWindow.newClickedEvent $ onNewClicked env
        gui `onGui` MainWindow.openClickedEvent $ onOpenClicked env
        gui `onGui` MainWindow.digestClickedEvent $ onDigestClicked env
        gui `onGui` MainWindow.saveClickedEvent $ onSaveClicked env
        gui `onGui` MainWindow.saveProjectClickedEvent $ onSaveProjectClicked env
        gui `onGui` MainWindow.declClickedEvent $ onDeclClicked env
        gui `onGui` MainWindow.declViewClickedEvent $ onDeclViewClicked env gui
        gui `onGui` MainWindow.buildClickedEvent $ onBuildClicked env
        gui `onGui` MainWindow.runClickedEvent $ onRunClicked env
        gui `onGui` MainWindow.windowClosedEvent $ do
            liftIO exitSuccess
            return False
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (CabalProject (ProjectStateT IO)))
              (Unopened, Project.empty)
