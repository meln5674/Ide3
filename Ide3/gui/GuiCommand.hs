{-# LANGUAGE PolyKinds, ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
module GuiCommand where

import System.Directory

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

import GuiClass
import GuiClass.GuiEnv

import SearchMode

import qualified GuiCommand.Internal as Internal
import GuiCommand.Internal (UserError, {-DialogOnErrorArg,-} GuiCommand )

type GuiCommand2 proxy m p m' = 
    ( ViewerMonad m
    , InteruptMonad1 (MVar (MVarType p)) m
    , ErrorClass m
    , MonadIO m'
    )

dialogOnError 
    :: forall proxy m p m' a
     . GuiCommand2 proxy m p m'
    => a
    -> GuiEnvT proxy m p (SolutionResult UserError m) a
    -> GuiEnvT proxy m p m' a
dialogOnError default_ f = do
    env <- getEnv
    withSolutionMVar $ \var -> liftIO $ do
        interupt1 var $ do
            r <- runExceptT $ runGuiEnvT f env
            case r of
                Right x -> return x
                Left e -> do
                    {-dialog <- messageDialogNew
                        Nothing
                        []
                        MessageError
                        ButtonsClose
                        (show e)
                    _ <- dialogRun dialog
                    widgetDestroy dialog-}
                    displayError $ show e
                    return default_

dialogOnErrorConc 
    :: forall proxy m p m'
     . GuiCommand2 proxy m p m'
    => GuiEnvT proxy m p (SolutionResult UserError m) ()
    -> GuiEnvT proxy m p m' ThreadId
dialogOnErrorConc f = do
    env <- getEnv
    withSolutionMVar $ \var -> liftIO $ forkIO $ do
        interupt1 var $ do
            r <- runExceptT $ runGuiEnvT f env
            case r of
                Right () -> return ()
                Left e -> do
                    {-dialog <- messageDialogNew
                        Nothing
                        []
                        MessageError
                        ButtonsClose
                        (show e)
                    _ <- dialogRun dialog
                    widgetDestroy dialog-}
                    displayError $ show e
                
doError :: ( GuiCommand (GuiEnvT proxy m p) m
           , GuiCommand2 proxy m p m'
           )
        => SolutionError UserError
        -> GuiEnvT proxy m p m' ()
doError e = dialogOnError () $ Internal.doError e

doNew :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m', MonadIO m )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> GuiEnvT proxy m p m' ()
doNew maybeSolutionRoot projectName templateName 
    = dialogOnError () $ Internal.doNew maybeSolutionRoot projectName templateName 

doOpen :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m', MonadIO m )
       => FilePath
       -> GuiEnvT proxy m p m' ()
doOpen path = dialogOnError () $ Internal.doOpen path

doGetDecl :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
          => TreePath
          -> GuiEnvT proxy m p m' ()
doGetDecl path = dialogOnError () $ Internal.doGetDecl path

doBuild :: ( GuiCommand (GuiEnvT proxy m p) m
           , GuiCommand2 proxy m p m' 
           , MonadIO m
           , MonadMask m
           )
        => GuiEnvT proxy m p m' ThreadId
doBuild = dialogOnErrorConc $ Internal.doBuild

doRun :: ( GuiCommand (GuiEnvT proxy m p) m 
         , GuiCommand2 proxy m p m' 
         , MonadMask m
         , MonadIO m
         )
      => GuiEnvT proxy m p m' ()
doRun = dialogOnError () $ Internal.doRun


doSave :: ( GuiCommand (GuiEnvT proxy m p) m 
          , MonadMask m
          , GuiCommand2 proxy m p m' 
          )
        => GuiEnvT proxy m p m' ()
doSave = dialogOnError () $ Internal.doSave
                

doSaveSolution :: ( GuiCommand (GuiEnvT proxy m p) m
                  , MonadMask m
                  , GuiCommand2 proxy m p m' 
                  )
              => Maybe FilePath
              -> GuiEnvT proxy m p m' ()
doSaveSolution path = dialogOnError () $ Internal.doSaveSolution path

doAddModule :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
            => ProjectInfo
            -> ModuleInfo
            -> GuiEnvT proxy m p m' ()
doAddModule pi mi = dialogOnError () $ Internal.doAddModule pi mi

doRemoveModule :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
               => ProjectInfo
               -> ModuleInfo
               -> GuiEnvT proxy m p m' ()
doRemoveModule pi mi = dialogOnError () $ Internal.doRemoveModule pi mi


doAddDeclaration :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> GuiEnvT proxy m p m' ()
doAddDeclaration pi mi di = dialogOnError () $ Internal.doAddDeclaration pi mi di

doRemoveDeclaration :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> GuiEnvT proxy m p m' ()
doRemoveDeclaration pi mi di = dialogOnError () $ Internal.doRemoveDeclaration pi mi di

doUnExportDeclaration :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
                      => ProjectInfo
                      -> ModuleInfo
                      -> DeclarationInfo
                      -> GuiEnvT proxy m p m' ()
doUnExportDeclaration pi mi di = dialogOnError () $ Internal.doUnExportDeclaration pi mi di

doAddImport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> GuiEnvT proxy m p m' (Maybe (SolutionError UserError))
doAddImport pi mi importStr = dialogOnError Nothing $ Internal.doAddImport pi mi importStr

doRemoveImport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m'  )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> GuiEnvT proxy m p m' ()
doRemoveImport pi mi ii = dialogOnError () $ Internal.doRemoveImport pi mi ii

doGetImport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> GuiEnvT proxy m p  m' (Maybe String)
doGetImport pi mi ii = dialogOnError Nothing $ Internal.doGetImport pi mi ii

doEditImport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> String
             -> GuiEnvT proxy m p  m' (Maybe (SolutionError UserError))
doEditImport pi mi ii importStr = dialogOnError Nothing $ Internal.doEditImport pi mi ii importStr

doAddExport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> GuiEnvT proxy m p  m' (Maybe (SolutionError UserError))
doAddExport pi mi exportStr = dialogOnError Nothing $ Internal.doAddExport pi mi exportStr

doRemoveExport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> GuiEnvT proxy m p  m' ()
doRemoveExport pi mi ei = dialogOnError () $ Internal.doRemoveExport pi mi ei


doGetExport :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> GuiEnvT proxy m p  m' (Maybe String)
doGetExport pi mi ei = dialogOnError Nothing $ Internal.doGetExport pi mi ei

doEditExport :: ( GuiCommand (GuiEnvT proxy m p) m
                , GuiCommand2 proxy m p m'
                )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> String
             -> GuiEnvT proxy m p  m' (Maybe (SolutionError UserError))
doEditExport pi mi ei importStr = dialogOnError Nothing $ Internal.doEditExport pi mi ei importStr

doExportAll :: ( GuiCommand (GuiEnvT proxy m p) m, GuiCommand2 proxy m p m' )
            => ProjectInfo
            -> ModuleInfo
            -> GuiEnvT proxy m p  m' ()
doExportAll pi mi = dialogOnError () $ Internal.doExportAll pi mi

doSearch :: ( GuiCommand (GuiEnvT proxy m p) m
            , GuiCommand2 proxy m p m'
            )
         => GuiEnvT proxy m p m' ()
doSearch = dialogOnError () $ Internal.doSearch

doSetSearchMode :: ( GuiCommand (GuiEnvT proxy m p) m
                   , GuiCommand2 proxy m p m'
                   )
                => SearchMode
                -> GuiEnvT proxy m p m' ()
doSetSearchMode mode = dialogOnError () $ Internal.doSetSearchMode mode

doGotoDeclaration
    :: ( GuiCommand (GuiEnvT proxy m p) m
       , GuiCommand2 proxy m p m'
       )
    => GuiEnvT proxy m p m' ()
doGotoDeclaration = dialogOnError () $ Internal.doGotoDeclaration
