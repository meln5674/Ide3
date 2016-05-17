{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}
module GuiCommand where

import Graphics.UI.Gtk

import Data.Proxy

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Monad

import Builder

import Viewer
import ViewerMonad2

import GuiEnv
import GuiMonad
import ProjectTree

type UserError = ()

dialogOnError :: (ViewerMonad m, InteruptMonad2 p m, TextBufferClass buffer)
              => GuiEnv proxy m p buffer
              -> a
              -> ProjectResult (ViewerStateT m) UserError a
              -> IO a
dialogOnError env default_ f = withProjectMVar env $ \var -> do
    r <- interupt1 var (runExceptT f)
    case r of
        Right x -> do
            return x
        Left e -> do
            dialog <- messageDialogNew
                Nothing
                []
                MessageError
                ButtonsClose
                (show e)
            dialogRun dialog
            widgetDestroy dialog
            return default_

doOpen :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
       => GuiEnv proxy m p buffer
       -> FilePath
       -> IO ()
doOpen env path = dialogOnError env () $ 
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $ do
        openProject path
        withGuiComponents env $ flip withProjectTree populateTree

doGetDecl :: forall proxy m buffer p 
          . ( MonadIO m
            , ViewerMonad m
            , TextBufferClass buffer
            , InteruptMonad2 p m
            )
          => GuiEnv proxy m p buffer
          -> TreePath
          -> TreeViewColumn 
          -> IO ()
doGetDecl env path column = dialogOnError env () $ 
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $ 
        withGuiComponents env $ \comp -> do
            index <- liftIO $ withProjectTree comp $ getModuleAndDecl path
            case index of
                Just (mi, di) -> do
                        decl <- getDeclaration mi di
                        let text = body decl
                        liftIO $ withEditorBuffer comp $ flip textBufferSetText text
                _ -> return ()

doBuild :: forall proxy m buffer p 
         . ( MonadIO m
           , ViewerMonad m
           , TextBufferClass buffer
           , InteruptMonad2 p m
           , MonadMask m
           )
         => GuiEnv proxy m p buffer
         -> IO ()
doBuild env = dialogOnError env () $ 
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $ 
        withGuiComponents env $ \comp -> do
            prepareBuild
            r <- ExceptT $ lift $ runExceptT $ runBuilder stackBuilder
            let text = case r of
                    BuildSucceeded out err -> out ++ err
                    BuildFailed out err -> out ++ err
            liftIO $ withBuildBuffer comp $ flip textBufferSetText text
