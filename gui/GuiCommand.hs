{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}
module GuiCommand where

import System.Directory
import System.FilePath

import Graphics.UI.Gtk

import Data.Proxy

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Monad

import qualified Ide3.Declaration as Declaration

import Builder
import Initializer
import Runner

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

doNew :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
       => GuiEnv proxy m p buffer
       -> Maybe FilePath
       -> String
       -> Maybe String
       -> IO ()
doNew env projectRoot projectName templateName = dialogOnError env () $ do
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $ do
        case projectRoot of
            Nothing -> throwE $ InvalidOperation "Please choose a directory" ""
            Just projectRoot -> do
                liftIO $ setCurrentDirectory projectRoot
                createNewFile $ projectName ++ ".proj"
                r <- ExceptT 
                        $ lift 
                        $ runExceptT 
                        $ runInitializer stackInitializer 
                                        (StackInitializerArgs projectName templateName)
                case r of
                    InitializerSucceeded out err -> do
                        withGuiComponents env $ flip withProjectTree populateTree
                        saveProject Nothing
                    InitializerFailed out err -> throwE $ InvalidOperation (out ++ err) ""

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
                        lift $ setCurrentDecl mi di
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

doRun :: forall proxy m buffer p
       . ( MonadIO m
         , ViewerMonad m
         , TextBufferClass buffer
         , InteruptMonad2 p m
         , MonadMask m
         )
         => GuiEnv proxy m p buffer
         -> IO ()
doRun env = dialogOnError env () $
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $
        withGuiComponents env $ \comp -> do
            r <- ExceptT $ lift $ runExceptT $ runRunner stackRunner
            let text = case r of
                    RunSucceeded out err -> out ++ err
                    RunFailed out err -> out ++ err
            liftIO $ withBuildBuffer comp $ flip textBufferSetText text


doSave :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
        => GuiEnv proxy m p buffer
        -> IO ()
doSave env = dialogOnError env () $ 
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $
        withGuiComponents env $ \comp -> do
            mod <- lift $ gets currentModule
            decl <- lift $ gets currentDecl
            case (mod,decl) of
                (Just mi, Just di) -> do
                    text <- liftIO $ withEditorBuffer comp $ \buffer -> do
                        start <- textBufferGetStartIter buffer
                        end <- textBufferGetEndIter buffer
                        textBufferGetText buffer start end False
                    
                    editDeclaration mi di (\_ -> Declaration.parseAndCombine text Nothing)
                    saveProject Nothing
                _ -> return ()
                

doSaveProject :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
              => GuiEnv proxy m p buffer
              -> Maybe FilePath
              -> IO ()
doSaveProject env path = dialogOnError env () $
    flip asTypeOf (undefined :: ProjectResult (ViewerStateT m) UserError ()) $
        saveProject path
