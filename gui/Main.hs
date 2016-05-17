{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, PolyKinds #-}
module Main where

import Data.Tree
import Data.Proxy

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

import ReadOnlyFilesystemProject


makeMainWindowWith f = do
    initGUI
    window <- windowNew
    f window
    widgetShowAll window
    putStrLn "Starting"
    mainGUI

makeRenderer = cellRendererTextNew

makeProjView treeStore renderer container = do
    treeViewColumn <- treeViewColumnNew
    projView <- treeViewNewWithModel treeStore
    treeViewAppendColumn projView treeViewColumn
    cellLayoutPackStart treeViewColumn renderer False
    cellLayoutSetAttributes treeViewColumn renderer treeStore $ renderProjectTreeElem
    tableAttach
        container
        projView
        0 1
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
    return projView

makeDeclView buffer container = do
    declView <- textViewNewWithBuffer buffer
    tableAttach
        container
        declView
        1 2
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
    return declView
    
makeOpenButton container = do
    openButton <- buttonNew
    buttonSetLabel openButton "Open"
    tableAttach
        container
        openButton
        0 2
        1 2
        [Expand,Fill] [Expand,Fill] 0 0
    return openButton

makeContainerWith f window = do
    --aligner <- hBoxNew True 0
    container <- tableNew 2 2 False
    --window `containerAdd` aligner
    --aligner `containerAdd` container
    window `containerAdd` container
    f container

{-
withState :: (Show u, ViewerMonad m, InteruptMonad1 (MVar p) m)
          => MVar p
          -> ProjectResult m u a 
          -> IO (Maybe a)
withState var f = do
    r <- interupt1 var (runExceptT f)
    case r of
        Right x -> do
            liftIO $ putStrLn "Finishing state success"
            return $ Just x
        Left e -> do
            liftIO $ putStrLn "Finishing state failure"
            dialog <- messageDialogNew
                Nothing
                []
                MessageError
                ButtonsClose
                (show e)
            dialogRun dialog
            --dialogDestroy dialog
            widgetDestroy dialog
            liftIO $ putStrLn "Closed"
            return Nothing

withStateMaybe :: (Show u, ViewerMonad m, InteruptMonad1 (MVar p) m)
               => a 
               -> MVar p
               -> ProjectResult m u a 
               -> IO a
withStateMaybe default_ var f = do
    x <- withState var f
    return $ maybe default_ id x

onDeclClicked' :: (TextBufferClass self, ViewerMonad m, MonadIO m, InteruptMonad1 (MVar p) m)
               => self
               -> TreeStore ProjectTreeElem 
               -> MVar p
               -> TreePath
               -> TreeViewColumn 
               -> ProjectResult m UserError ()
onDeclClicked' buffer treeStore _ path col = do
    index <- liftIO $ getModuleAndDecl path treeStore
    case index of
        Just (mi, di) -> do
                decl <- getDeclaration mi di
                let text = body decl
                liftIO $ textBufferSetText buffer text
        _ -> return ()    

onDeclClicked :: TextBufferClass self
              => self
              -> TreeStore ProjectTreeElem 
              -> MVar Project
              -> TreePath
              -> TreeViewColumn 
              -> IO ()
onDeclClicked buffer treeStore projectMVar path col = withStateMaybe () projectMVar $ 
    onDeclClicked' buffer treeStore projectMVar path col


onOpenClicked :: TreeStore ProjectTreeElem 
              -> MVar Project 
              -> IO ()
onOpenClicked treeStore projectMVar = do
    liftIO $ putStrLn "onOpenClicked"
    withStateMaybe () projectMVar $ do
        let f :: ProjectResult (ProjectStateT IO) () ()
            f = do
                liftIO $ putStrLn "Opening Dialog"
                dialog <- liftIO $ fileChooserDialogNew
                    Nothing
                    Nothing
                    FileChooserActionSelectFolder
                    [("Open",ResponseAccept),("Close",ResponseReject)]
                liftIO $ putStrLn "Test"
                r <- liftIO $ dialogRun dialog
                case r of
                    ResponseAccept -> do
                        Just path <- liftIO $ fileChooserGetFilename dialog
                        digestProject path
                        populateTree treeStore
                        liftIO $ widgetDestroy dialog
                    ResponseReject -> liftIO $ widgetDestroy dialog
                    _ -> return ()
        f
-}

doMain :: forall proxy m p 
        . (MonadIO m, ViewerMonad m, InteruptMonad2 p m) 
       => proxy m 
       -> p
       -> IO ()
doMain proxy init = do
    projectMVar <- newMVar (Viewer Nothing, init)
    makeMainWindowWith $ makeContainerWith $ \container -> do
        components <- initializeComponents
        renderer <- makeRenderer
        projView <- withProjectTree components $ \treeStore
            -> makeProjView treeStore renderer container
        declView <- withEditorBuffer components $ \buffer
            -> makeDeclView buffer container 
        openButton <- makeOpenButton container
        
        
        openButton `on` buttonPressEvent $ do
            liftIO $ do
                dialog <- liftIO $ fileChooserDialogNew
                    Nothing
                    Nothing
                    FileChooserActionSelectFolder
                    [("Open",ResponseAccept),("Close",ResponseReject)]
                liftIO $ putStrLn "Test"
                r <- liftIO $ dialogRun dialog
                case r of
                    ResponseAccept -> do
                        Just path <- liftIO $ fileChooserGetFilename dialog
                        doOpen proxy components path projectMVar
                        liftIO $ widgetDestroy dialog
                    ResponseReject -> liftIO $ widgetDestroy dialog
                    _ -> return ()
            return False
        
        {-
        projView `on` rowActivated $ onDeclClicked buffer treeStore projectMVar
        -}
        
        return ()


main :: IO ()
main = doMain (Proxy :: Proxy (ReadOnlyFilesystemProjectT (ProjectStateT IO)))
              (Unopened, Project.empty)

{-
type X a = StateT Int (StateT Bool IO) a

type Y a = StateT Int (StateT Bool (StateT Int IO)) a

foo :: X ()
foo = do
    modify (+1)
    lift $ modify not
    return ()

bar :: Y ()
bar = do
    modify (+1)
    lift $ modify not
    lift $ lift $ modify (subtract 1)
    return ()

main :: IO ()
main = do
    var <- newMVar (0 :: Int,(False,0 :: Int))
    interupt1 var bar
    interupt1 var $ do
        i <- get
        b <- lift $ get
        j <- lift $ lift $ get
        liftIO $ print i
        liftIO $ print b
        liftIO $ print j
        return () :: Y ()
-}
