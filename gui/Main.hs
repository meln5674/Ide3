{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PolyKinds #-}
module Main where

import Data.Tree
import Data.Proxy

import System.Exit
import System.Directory
import System.FilePath

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

import Signal

import qualified MainWindow
import qualified NewProjectDialog

import NewProjectDialog (NewProjectDialog)

--import ReadOnlyFilesystemProject
import SimpleFilesystemProject

import Initializer

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
        gui `onGui` MainWindow.openClickedEvent $ onOpenClicked env
        gui `onGui` MainWindow.declClickedEvent $ onDeclClicked env
        gui `onGui` MainWindow.buildClickedEvent $ onBuildClicked env
        gui `onGui` MainWindow.saveClickedEvent $ onSaveClicked env
        gui `onGui` MainWindow.saveProjectClickedEvent $ onSaveProjectClicked env
        gui `onGui` MainWindow.newClickedEvent $ onNewClicked env
        gui `onGui` MainWindow.windowClosedEvent $ do
            liftIO $ exitSuccess
            return False
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (SimpleFilesystemProjectT (ProjectStateT IO)))
              (Unopened, Project.empty)
