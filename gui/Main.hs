{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PolyKinds #-}
module Main where

import Data.Tree
import Data.Proxy

import System.Directory

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
import GuiLayout
import GuiEnv

import ReadOnlyFilesystemProject


onOpenClicked :: forall proxy m p buffer
               . ( MonadIO m
                 , ViewerMonad m
                 , InteruptMonad2 p m
                 , TextBufferClass buffer
                 )
              => GuiEnv proxy m p buffer 
              -> EventM EButton Bool
onOpenClicked env = liftIO $ do
        dialog <- liftIO $ fileChooserDialogNew
            Nothing
            Nothing
            FileChooserActionSelectFolder
            [("Open",ResponseAccept),("Close",ResponseReject)]
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
    projectMVar <- newMVar (Viewer Nothing, init)
    components <- initializeComponents
    let env = GuiEnv proxy components projectMVar
    makeGui env $ \gui -> do
        withOpenButton gui $ \openButton -> openButton `on` buttonPressEvent $ onOpenClicked env
        withProjectView gui $ \projView -> projView `on` rowActivated $ onDeclClicked env
        withBuildButton gui $ \buildButton -> buildButton `on` buttonPressEvent $ onBuildClicked env
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (ReadOnlyFilesystemProjectT (ProjectStateT IO)))
              (Unopened, Project.empty)
