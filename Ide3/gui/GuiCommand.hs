{-# LANGUAGE PolyKinds, ConstraintKinds #-}
module GuiCommand where

import System.Directory

import Graphics.UI.Gtk

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Utils
import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Import as Import

import Builder
import Initializer
import Runner

import Viewer
import ViewerMonad2

import GuiEnv
import GuiMonad
import ProjectTree

import qualified GuiCommand.Internal as Internal
import GuiCommand.Internal (UserError, DialogOnErrorArg, GuiCommand )


dialogOnError :: (ViewerMonad m, InteruptMonad2 p m, TextBufferClass buffer)
              => a
              -> DialogOnErrorArg proxy m p buffer a
              -> GuiEnvT proxy m p buffer IO a
dialogOnError default_ f = do
    env <- getEnv
    withProjectMVar $ \var -> liftIO $ do
        r <- interupt1 var $ runExceptT $ runGuiEnvT f env
        case r of
            Right x -> return x
            Left e -> do
                dialog <- messageDialogNew
                    Nothing
                    []
                    MessageError
                    ButtonsClose
                    (show e)
                _ <- dialogRun dialog
                widgetDestroy dialog
                return default_

dialogOnErrorConc :: (ViewerMonad m, InteruptMonad2 p m, TextBufferClass buffer)
                  => DialogOnErrorArg proxy m p buffer ()
                  -> GuiEnvT proxy m p buffer IO ThreadId
dialogOnErrorConc f = do
    env <- getEnv
    withProjectMVar $ \var -> liftIO $ forkIO $ do
        r <- interupt1 var $ runExceptT $ runGuiEnvT f env
        case r of
            Right () -> return ()
            Left e -> do
                dialog <- messageDialogNew
                    Nothing
                    []
                    MessageError
                    ButtonsClose
                    (show e)
                _ <- dialogRun dialog
                widgetDestroy dialog
                
doError :: ( GuiCommand m p buffer )
        => ProjectError UserError
        -> GuiEnvT proxy m p buffer IO ()
doError e = dialogOnError () $ Internal.doError e

doNew :: ( GuiCommand m p buffer )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> GuiEnvT proxy m p buffer IO ()
doNew maybeProjectRoot projectName templateName 
    = dialogOnError () $ Internal.doNew maybeProjectRoot projectName templateName 

doOpen :: ( GuiCommand m p buffer )
       => FilePath
       -> GuiEnvT proxy m p buffer IO ()
doOpen path = dialogOnError () $ Internal.doOpen path

doGetDecl :: ( GuiCommand m p buffer )
          => TreePath
          -> TreeViewColumn 
          -> GuiEnvT proxy m p buffer IO ()
doGetDecl path col = dialogOnError () $ Internal.doGetDecl path col

doBuild :: ( GuiCommand m p buffer
           , MonadMask m
           )
        => GuiEnvT proxy m p buffer IO ThreadId
doBuild = dialogOnErrorConc $ Internal.doBuild

doRun :: ( GuiCommand m p buffer
         , MonadMask m
         )
      => GuiEnvT proxy m p buffer IO ()
doRun = dialogOnError () $ Internal.doRun


doSave :: ( GuiCommand m p buffer
          , MonadMask m
          )
        => GuiEnvT proxy m p buffer IO ()
doSave = dialogOnError () $ Internal.doSave
                

doSaveProject :: ( GuiCommand m p buffer
                 , MonadMask m
                 )
              => Maybe FilePath
              -> GuiEnvT proxy m p buffer IO ()
doSaveProject path = dialogOnError () $ Internal.doSaveProject path

doAddModule :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> GuiEnvT proxy m p buffer IO ()
doAddModule mi = dialogOnError () $ Internal.doAddModule mi

doRemoveModule :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> GuiEnvT proxy m p buffer IO ()
doRemoveModule mi = dialogOnError () $ Internal.doRemoveModule mi


doAddDeclaration :: ( GuiCommand m p buffer )
                 => ModuleInfo
                 -> DeclarationInfo
                 -> GuiEnvT proxy m p buffer IO ()
doAddDeclaration mi di = dialogOnError () $ Internal.doAddDeclaration mi di

doRemoveDeclaration :: ( GuiCommand m p buffer )
                    => ModuleInfo
                    -> DeclarationInfo
                    -> GuiEnvT proxy m p buffer IO ()
doRemoveDeclaration mi di = dialogOnError () $ Internal.doRemoveDeclaration mi di

doUnExportDeclaration :: ( GuiCommand m p buffer )
                      => ModuleInfo
                      -> DeclarationInfo
                      -> GuiEnvT proxy m p buffer IO ()
doUnExportDeclaration mi di = dialogOnError () $ Internal.doUnExportDeclaration mi di

doAddImport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> String
            -> GuiEnvT proxy m p buffer IO (Maybe (ProjectError UserError))
doAddImport mi importStr = dialogOnError Nothing $ Internal.doAddImport mi importStr

doRemoveImport :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> ImportId
               -> GuiEnvT proxy m p buffer IO ()
doRemoveImport mi ii = dialogOnError () $ Internal.doRemoveImport mi ii

doGetImport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> ImportId
            -> GuiEnvT proxy m p buffer IO (Maybe String)
doGetImport mi ii = dialogOnError Nothing $ Internal.doGetImport mi ii

doEditImport :: ( GuiCommand m p buffer )
             => ModuleInfo
             -> ImportId
             -> String
             -> GuiEnvT proxy m p buffer IO (Maybe (ProjectError UserError))
doEditImport mi ii importStr = dialogOnError Nothing $ Internal.doEditImport mi ii importStr

doAddExport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> String
            -> GuiEnvT proxy m p buffer IO (Maybe (ProjectError UserError))
doAddExport mi exportStr = dialogOnError Nothing $ Internal.doAddExport mi exportStr

doRemoveExport :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> ExportId
               -> GuiEnvT proxy m p buffer IO ()
doRemoveExport mi ei = dialogOnError () $ Internal.doRemoveExport mi ei


doGetExport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> ExportId
            -> GuiEnvT proxy m p buffer IO (Maybe String)
doGetExport mi ei = dialogOnError Nothing $ Internal.doGetExport mi ei

doEditExport :: ( GuiCommand m p buffer )
             => ModuleInfo
             -> ExportId
             -> String
             -> GuiEnvT proxy m p buffer IO (Maybe (ProjectError UserError))
doEditExport mi ei importStr = dialogOnError Nothing $ Internal.doEditExport mi ei importStr
