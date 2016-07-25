{-# LANGUAGE ConstraintKinds #-}
module GuiCommand.Internal where

import System.Directory

import Graphics.UI.Gtk

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Utils
import Ide3.Monad

import qualified Ide3.Declaration as Declaration
import qualified Ide3.Import as Import
import qualified Ide3.Export as Export

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

type GuiCommand m p buffer
    = ( MonadIO m
      , ViewerMonad m
      , TextBufferClass buffer
      , InteruptMonad2 p m
      )

doError :: ( GuiCommand m p buffer ) => ProjectError UserError -> DialogOnErrorArg proxy m p buffer a
doError = lift . throwE 


doNew ::  ( GuiCommand m p buffer )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> DialogOnErrorArg proxy m p buffer ()
doNew maybeProjectRoot projectName templateName = do
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

doOpen :: ( GuiCommand m p buffer )
       => FilePath
       -> DialogOnErrorArg proxy m p buffer ()
doOpen path = do
    lift $ openProject path
    withGuiComponents $ lift . flip withProjectTree populateTree


doGetDecl :: ( GuiCommand m p buffer )
          => TreePath
          -> TreeViewColumn 
          -> DialogOnErrorArg proxy m p buffer ()
doGetDecl path _ = withGuiComponents $ \comp -> do
    index <- lift $ wrapIOError $ withProjectTree comp $ findAtPath path
    case index of
        DeclResult mi di -> do
                decl <- lift $ getDeclaration mi di
                lift $ wrapIOError $ comp `setDeclBufferText` body decl
                lift $ lift $ setCurrentDecl mi di
        _ -> return ()

doBuild :: ( GuiCommand m p buffer
           , MonadMask m
           )
        => DialogOnErrorArg proxy m p buffer ()
doBuild = withGuiComponents $ \comp -> lift $ do
    prepareBuild
    r <- ExceptT $ lift $ runExceptT $ runBuilder stackBuilder
    let text = case r of
            BuildSucceeded out err -> out ++ err
            BuildFailed out err -> out ++ err
    wrapIOError $ withBuildBuffer comp $ flip textBufferSetText text

doRun :: ( GuiCommand m p buffer
         , MonadMask m
         )
      => DialogOnErrorArg proxy m p buffer ()
doRun = withGuiComponents $ \comp -> lift $ do
    r <- ExceptT $ lift $ runExceptT $ runRunner stackRunner
    let text = case r of
            RunSucceeded out err -> out ++ err
            RunFailed out err -> out ++ err
    wrapIOError $ withBuildBuffer comp $ flip textBufferSetText text


doSave :: ( GuiCommand m p buffer
          , MonadMask m
          )
       => DialogOnErrorArg proxy m p buffer ()
doSave = withGuiComponents $ \comp -> lift $ do
    m <- lift $ gets currentModule
    d <- lift $ gets currentDecl
    case (m,d) of
        (Just mi, Just di) -> do
            text <- wrapIOError $ withEditorBuffer comp $ \buffer -> do
                start <- textBufferGetStartIter buffer
                end <- textBufferGetEndIter buffer
                textBufferGetText buffer start end False
            
            di' <- editDeclaration mi di $ const $ Declaration.parseAndCombine text Nothing
            withProjectTree comp populateTree
            saveProject Nothing
            when (di /= di') $ do
                lift $ setCurrentDecl mi di'
        _ -> return ()
        

doSaveProject :: ( GuiCommand m p buffer
                 , MonadMask m
                 )
              => Maybe FilePath
              -> DialogOnErrorArg proxy m p buffer ()
doSaveProject path = lift $ saveProject path

doAddModule :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> DialogOnErrorArg proxy m p buffer ()
doAddModule mi = do
    lift $ createModule mi
    withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree

doRemoveModule :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveModule mi = do
    lift $ removeModule mi
    withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree


doAddDeclaration :: ( GuiCommand m p buffer )
                 => ModuleInfo
                 -> DeclarationInfo
                 -> DialogOnErrorArg proxy m p buffer ()
doAddDeclaration mi di = do
    let newdecl = WithBody (UnparseableDeclaration di) ""
    lift $ addDeclaration mi newdecl
    withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree

doRemoveDeclaration :: ( GuiCommand m p buffer )
                    => ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doRemoveDeclaration mi di = do
    lift $ removeDeclaration mi di
    withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree

doUnExportDeclaration :: ( GuiCommand m p buffer )
                    => ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doUnExportDeclaration mi (DeclarationInfo sym) = do
    matches <- lift $ do
        es <- do
            m <- getModule mi
            maybeEis <- getExports mi
            case maybeEis of
                Nothing -> throwE $ InvalidOperation "All symbols are exported" ""
                Just eis -> 
                    forM eis $ \ei -> do
                        (WithBody e _) <- getExport mi ei
                        syms <- Export.symbolsProvided m e
                        return (ei,syms)
        return $ flip filter es $ \(_,syms) -> sym `elem` syms
    case matches of
        [] -> lift $ throwE $ SymbolNotExported mi sym ""
        [(ei,syms)] -> do
            case syms of
                [] -> lift $ throwE $ InvalidOperation "Internal Error" "doUnExportDeclaration"
                [_] -> do
                    lift $ removeExport mi ei
                    withGuiComponents $ \comp -> lift $ withProjectTree comp populateTree
                _ -> lift $ throwE $ Unsupported "Symbol is exported with other symbols, please remove export manually"
        _ -> lift $ throwE $ Unsupported "Multiple exports found, please remove exports manually"

doAddImport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> String
            -> DialogOnErrorArg proxy m p buffer (Maybe (ProjectError UserError))
doAddImport mi importStr = do
    case Import.parse importStr of
        Right newImport -> do
            _ <- lift $ addImport mi (WithBody newImport importStr)
            withGuiComponents $ lift . flip withProjectTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveImport :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> ImportId
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveImport mi ii = do
    lift $ removeImport mi ii
    withGuiComponents $ lift . flip withProjectTree populateTree


doGetImport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> ImportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetImport mi ii = do
    (WithBody _ b) <- lift $ getImport mi ii
    return $ Just b

doEditImport :: ( GuiCommand m p buffer )
             => ModuleInfo
             -> ImportId
             -> String
             -> DialogOnErrorArg proxy m p buffer (Maybe (ProjectError UserError))
doEditImport mi ii importStr = do
    case Import.parse importStr of
        Right newImport -> do
            lift $ removeImport mi ii
            _ <- lift $ addImport mi (WithBody newImport importStr)
            withGuiComponents $ lift . flip withProjectTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doAddExport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> String
            -> DialogOnErrorArg proxy m p buffer (Maybe (ProjectError UserError))
doAddExport mi exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            _ <- lift $ addExport mi (WithBody newExport exportStr)
            withGuiComponents $ lift . flip withProjectTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveExport :: ( GuiCommand m p buffer )
               => ModuleInfo
               -> ExportId
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveExport mi ei = do
    lift $ removeExport mi ei
    withGuiComponents $ lift . flip withProjectTree populateTree


doGetExport :: ( GuiCommand m p buffer )
            => ModuleInfo
            -> ExportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetExport mi ei = do
    (WithBody _ b) <- lift $ getExport mi ei
    return $ Just b

doEditExport :: ( GuiCommand m p buffer )
             => ModuleInfo
             -> ExportId
             -> String
             -> DialogOnErrorArg proxy m p buffer (Maybe (ProjectError UserError))
doEditExport mi ei exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            lift $ removeExport mi ei
            _ <- lift $ addExport mi (WithBody newExport exportStr)
            withGuiComponents $ lift . flip withProjectTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err
