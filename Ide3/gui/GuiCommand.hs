{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}
module GuiCommand where

import System.Directory

import Graphics.UI.Gtk

import Control.Monad.Catch

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

type UserError = ()

type DialogOnErrorArg proxy m p buffer a
    = GuiEnvT proxy m p buffer (ProjectResult (ViewerStateT m) UserError) a

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

doError :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
        => ProjectError UserError
        -> GuiEnvT proxy m p buffer IO ()
doError e = dialogOnError () $
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $
        lift $ throwE e

doNew :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
       => Maybe FilePath
       -> String
       -> Maybe String
       -> GuiEnvT proxy m p buffer IO ()
doNew maybeProjectRoot projectName templateName = dialogOnError () $ 
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $
        case maybeProjectRoot of
            Nothing -> lift $ throwE $ InvalidOperation "Please choose a directory" ""
            Just projectRoot -> do
                lift $ do
                    wrapIOError $ setCurrentDirectory projectRoot
                    createNewFile $ projectName ++ ".proj"
                r <- lift 
                        $ ExceptT 
                        $ lift 
                        $ runExceptT 
                        $ runInitializer stackInitializer 
                                        (StackInitializerArgs projectName templateName)
                case r of
                    InitializerSucceeded{} -> do
                        withGuiComponents $ lift . flip withProjectTree populateTree
                        lift $ saveProject Nothing
                    InitializerFailed out err -> lift $ throwE $ InvalidOperation (out ++ err) ""

doOpen :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          )
       => FilePath
       -> GuiEnvT proxy m p buffer IO ()
doOpen path = dialogOnError () $ 
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ do
        lift $ openProject path
        withGuiComponents $ lift . flip withProjectTree populateTree

doGetDecl :: forall proxy m buffer p 
          . ( MonadIO m
            , ViewerMonad m
            , TextBufferClass buffer
            , InteruptMonad2 p m
            )
          => TreePath
          -> TreeViewColumn 
          -> GuiEnvT proxy m p buffer IO ()
doGetDecl path _ = dialogOnError () $ 
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ 
        withGuiComponents $ \comp -> do
            index <- lift $ wrapIOError $ withProjectTree comp $ findAtPath path
            case index of
                DeclResult mi di -> do
                        decl <- lift $ getDeclaration mi di
                        let text = body decl
                        lift $ wrapIOError $ withEditorBuffer comp $ flip textBufferSetText text
                        lift $ lift $ setCurrentDecl mi di
                _ -> return ()

doBuild :: forall proxy m buffer p 
         . ( MonadIO m
           , ViewerMonad m
           , TextBufferClass buffer
           , InteruptMonad2 p m
           , MonadMask m
           )
         => GuiEnvT proxy m p buffer IO ()
doBuild = dialogOnError () $ 
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ 
        withGuiComponents $ \comp -> lift $ do
            prepareBuild
            r <- ExceptT $ lift $ runExceptT $ runBuilder stackBuilder
            let text = case r of
                    BuildSucceeded out err -> out ++ err
                    BuildFailed out err -> out ++ err
            wrapIOError $ withBuildBuffer comp $ flip textBufferSetText text

doRun :: forall proxy m buffer p
       . ( MonadIO m
         , ViewerMonad m
         , TextBufferClass buffer
         , InteruptMonad2 p m
         , MonadMask m
         )
      => GuiEnvT proxy m p buffer IO ()
doRun = dialogOnError () $
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $
        withGuiComponents $ \comp -> lift $ do
            r <- ExceptT $ lift $ runExceptT $ runRunner stackRunner
            let text = case r of
                    RunSucceeded out err -> out ++ err
                    RunFailed out err -> out ++ err
            wrapIOError $ withBuildBuffer comp $ flip textBufferSetText text


doSave :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
        => GuiEnvT proxy m p buffer IO ()
doSave = dialogOnError () $ 
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $
        withGuiComponents $ \comp -> lift $ do
            m <- lift $ gets currentModule
            d <- lift $ gets currentDecl
            case (m,d) of
                (Just mi, Just di) -> do
                    text <- wrapIOError $ withEditorBuffer comp $ \buffer -> do
                        start <- textBufferGetStartIter buffer
                        end <- textBufferGetEndIter buffer
                        textBufferGetText buffer start end False
                    
                    editDeclaration mi di (\_ -> Declaration.parseAndCombine text Nothing)
                    withProjectTree comp populateTree
                    saveProject Nothing
                _ -> return ()
                

doSaveProject :: forall proxy m buffer p 
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
              => Maybe FilePath
              -> GuiEnvT proxy m p buffer IO ()
doSaveProject path = dialogOnError () $
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $
        lift $ saveProject path

doAddModule :: forall proxy m buffer p
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
              => ModuleInfo
              -> GuiEnvT proxy m p buffer IO ()
doAddModule mi = dialogOnError () $ do
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ do
        lift $ createModule mi
        withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree

doRemoveModule :: forall proxy m buffer p
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
              => ModuleInfo
              -> GuiEnvT proxy m p buffer IO ()
doRemoveModule mi = dialogOnError () $ do
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ do
        lift $ removeModule mi
        withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree


doAddDeclaration :: forall proxy m buffer p
        . ( MonadIO m
          , ViewerMonad m
          , TextBufferClass buffer
          , InteruptMonad2 p m
          , MonadMask m
          )
              => ModuleInfo
              -> DeclarationInfo
              -> GuiEnvT proxy m p buffer IO ()
doAddDeclaration mi di = dialogOnError () $ do
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ do
        let newdecl = WithBody (UnparseableDeclaration di) ""
        lift $ addDeclaration mi newdecl
        withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree

doAddImport :: forall proxy m buffer p u
            . ( MonadIO m
              , ViewerMonad m
              , TextBufferClass buffer
              , InteruptMonad2 p m
              , MonadMask m
              )
            => ModuleInfo
            -> String
            -> GuiEnvT proxy m p buffer IO (Maybe (ProjectError UserError))
doAddImport mi importStr = dialogOnError Nothing $ do
    flip asTypeOf (undefined :: DialogOnErrorArg proxy m p buffer a) $ do
        case Import.parse importStr of
            Right newImport -> do
                lift $ addImport mi (WithBody newImport importStr)
                withGuiComponents $ lift . flip withProjectTree populateTree
                return Nothing
            Left parseError -> case parseError of
                err@ParseError{} -> return $ Just err
                err -> lift $ throwE err
