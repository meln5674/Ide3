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
import Ide3.NewMonad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Import as Import

import Builder
import Initializer
import Runner

import Viewer
import ViewerMonad2

import GuiEnv
import GuiMonad
import SolutionTree

import qualified GuiCommand.Internal as Internal
import GuiCommand.Internal (UserError, DialogOnErrorArg, GuiCommand )


dialogOnError :: (ViewerMonad m, InteruptMonad2 p m, TextBufferClass buffer)
              => a
              -> DialogOnErrorArg proxy m p buffer a
              -> GuiEnvT proxy m p buffer IO a
dialogOnError default_ f = do
    env <- getEnv
    withSolutionMVar $ \var -> liftIO $ do
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
    withSolutionMVar $ \var -> liftIO $ forkIO $ do
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
        => SolutionError UserError
        -> GuiEnvT proxy m p buffer IO ()
doError e = dialogOnError () $ Internal.doError e

doNew :: ( GuiCommand m p buffer )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> GuiEnvT proxy m p buffer IO ()
doNew maybeSolutionRoot projectName templateName 
    = dialogOnError () $ Internal.doNew maybeSolutionRoot projectName templateName 

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
                

doSaveSolution :: ( GuiCommand m p buffer
                 , MonadMask m
                 )
              => Maybe FilePath
              -> GuiEnvT proxy m p buffer IO ()
doSaveSolution path = dialogOnError () $ Internal.doSaveSolution path

doAddModule :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> GuiEnvT proxy m p buffer IO ()
doAddModule pi mi = dialogOnError () $ Internal.doAddModule pi mi

doRemoveModule :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> GuiEnvT proxy m p buffer IO ()
doRemoveModule pi mi = dialogOnError () $ Internal.doRemoveModule pi mi


doAddDeclaration :: ( GuiCommand m p buffer )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> GuiEnvT proxy m p buffer IO ()
doAddDeclaration pi mi di = dialogOnError () $ Internal.doAddDeclaration pi mi di

doRemoveDeclaration :: ( GuiCommand m p buffer )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> GuiEnvT proxy m p buffer IO ()
doRemoveDeclaration pi mi di = dialogOnError () $ Internal.doRemoveDeclaration pi mi di

doUnExportDeclaration :: ( GuiCommand m p buffer )
                      => ProjectInfo
                      -> ModuleInfo
                      -> DeclarationInfo
                      -> GuiEnvT proxy m p buffer IO ()
doUnExportDeclaration pi mi di = dialogOnError () $ Internal.doUnExportDeclaration pi mi di

doAddImport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> GuiEnvT proxy m p buffer IO (Maybe (SolutionError UserError))
doAddImport pi mi importStr = dialogOnError Nothing $ Internal.doAddImport pi mi importStr

doRemoveImport :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> GuiEnvT proxy m p buffer IO ()
doRemoveImport pi mi ii = dialogOnError () $ Internal.doRemoveImport pi mi ii

doGetImport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> GuiEnvT proxy m p buffer IO (Maybe String)
doGetImport pi mi ii = dialogOnError Nothing $ Internal.doGetImport pi mi ii

doEditImport :: ( GuiCommand m p buffer )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> String
             -> GuiEnvT proxy m p buffer IO (Maybe (SolutionError UserError))
doEditImport pi mi ii importStr = dialogOnError Nothing $ Internal.doEditImport pi mi ii importStr

doAddExport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> GuiEnvT proxy m p buffer IO (Maybe (SolutionError UserError))
doAddExport pi mi exportStr = dialogOnError Nothing $ Internal.doAddExport pi mi exportStr

doRemoveExport :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> GuiEnvT proxy m p buffer IO ()
doRemoveExport pi mi ei = dialogOnError () $ Internal.doRemoveExport pi mi ei


doGetExport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> GuiEnvT proxy m p buffer IO (Maybe String)
doGetExport pi mi ei = dialogOnError Nothing $ Internal.doGetExport pi mi ei

doEditExport :: ( GuiCommand m p buffer )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> String
             -> GuiEnvT proxy m p buffer IO (Maybe (SolutionError UserError))
doEditExport pi mi ei importStr = dialogOnError Nothing $ Internal.doEditExport pi mi ei importStr
