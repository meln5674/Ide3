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
import SolutionTree

type UserError = ()

type DialogOnErrorArg proxy m p buffer a
    = GuiEnvT proxy m p buffer (SolutionResult (ViewerStateT m) UserError) a

type GuiCommand m p buffer
    = ( MonadIO m
      , ViewerMonad m
      , TextBufferClass buffer
      , InteruptMonad2 p m
      )

doError :: ( GuiCommand m p buffer ) => SolutionError UserError -> DialogOnErrorArg proxy m p buffer a
doError = lift . throwE 


doNew ::  ( GuiCommand m p buffer )
      => Maybe FilePath
      -> String
      -> Maybe String
      -> DialogOnErrorArg proxy m p buffer ()
doNew maybeSolutionRoot projectName templateName = do
    case maybeSolutionRoot of
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
                    withGuiComponents $ lift . flip withSolutionTree populateTree
                    lift $ saveSolution Nothing
                InitializerFailed out err -> lift $ throwE $ InvalidOperation (out ++ err) ""

doOpen :: ( GuiCommand m p buffer )
       => FilePath
       -> DialogOnErrorArg proxy m p buffer ()
doOpen path = do
    lift $ openSolution path
    withGuiComponents $ lift . flip withSolutionTree populateTree


doGetDecl :: ( GuiCommand m p buffer )
          => TreePath
          -> TreeViewColumn 
          -> DialogOnErrorArg proxy m p buffer ()
doGetDecl path _ = withGuiComponents $ \comp -> do
    index <- lift $ wrapIOError $ withSolutionTree comp $ findAtPath path
    case index of
        DeclResult pi mi di -> do
                decl <- lift $ getDeclaration pi mi di
                lift $ wrapIOError $ comp `setDeclBufferText` body decl
                lift $ lift $ setCurrentDecl pi mi di
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
    p <- lift $ gets currentProject
    m <- lift $ gets currentModule
    d <- lift $ gets currentDecl
    case (p, m,d) of
        (Just pi, Just mi, Just di) -> do
            text <- wrapIOError $ withEditorBuffer comp $ \buffer -> do
                start <- textBufferGetStartIter buffer
                end <- textBufferGetEndIter buffer
                textBufferGetText buffer start end False
            
            di' <- editDeclaration pi mi di $ const $ Declaration.parseAndCombine text Nothing
            withSolutionTree comp populateTree
            saveSolution Nothing
            when (di /= di') $ do
                lift $ setCurrentDecl pi mi di'
        _ -> return ()
        

doSaveSolution :: ( GuiCommand m p buffer
                 , MonadMask m
                 )
              => Maybe FilePath
              -> DialogOnErrorArg proxy m p buffer ()
doSaveSolution path = lift $ saveSolution path

doAddModule :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> DialogOnErrorArg proxy m p buffer ()
doAddModule pi mi = do
    lift $ createModule pi mi
    withGuiComponents $ \comp -> lift $ withSolutionTree comp populateTree

doRemoveModule :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveModule pi mi = do
    lift $ removeModule pi mi
    withGuiComponents $ \comp -> lift $ withSolutionTree comp populateTree


doAddDeclaration :: ( GuiCommand m p buffer )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> DialogOnErrorArg proxy m p buffer ()
doAddDeclaration pi mi di = do
    let newdecl = WithBody (UnparseableDeclaration di) ""
    lift $ addDeclaration pi mi newdecl
    withGuiComponents $ \comp -> lift $ withSolutionTree comp populateTree

doRemoveDeclaration :: ( GuiCommand m p buffer )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doRemoveDeclaration pi mi di = do
    lift $ removeDeclaration pi mi di
    withGuiComponents $ \comp -> lift $ withSolutionTree comp populateTree

doUnExportDeclaration :: ( GuiCommand m p buffer )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doUnExportDeclaration pi mi (DeclarationInfo sym) = do
    matches <- lift $ do
        es <- do
            m <- getModule pi mi
            maybeEis <- getExports pi mi
            case maybeEis of
                Nothing -> throwE $ InvalidOperation "All symbols are exported" ""
                Just eis -> 
                    forM eis $ \ei -> do
                        (WithBody e _) <- getExport pi mi ei
                        syms <- Export.symbolsProvided pi m e
                        return (ei,syms)
        return $ flip filter es $ \(_,syms) -> sym `elem` syms
    case matches of
        [] -> lift $ throwE $ SymbolNotExported mi sym ""
        [(ei,syms)] -> do
            case syms of
                [] -> lift $ throwE $ InvalidOperation "Internal Error" "doUnExportDeclaration"
                [_] -> do
                    lift $ removeExport pi mi ei
                    withGuiComponents $ \comp -> lift $ withSolutionTree comp populateTree
                _ -> lift $ throwE $ Unsupported "Symbol is exported with other symbols, please remove export manually"
        _ -> lift $ throwE $ Unsupported "Multiple exports found, please remove exports manually"

doAddImport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> DialogOnErrorArg proxy m p buffer (Maybe (SolutionError UserError))
doAddImport pi mi importStr = do
    case Import.parse importStr of
        Right newImport -> do
            _ <- lift $ addImport pi mi (WithBody newImport importStr)
            withGuiComponents $ lift . flip withSolutionTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveImport :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> ImportId
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveImport pi mi ii = do
    lift $ removeImport pi mi ii
    withGuiComponents $ lift . flip withSolutionTree populateTree


doGetImport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetImport pi mi ii = do
    (WithBody _ b) <- lift $ getImport pi mi ii
    return $ Just b

doEditImport :: ( GuiCommand m p buffer )
             => ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> String
             -> DialogOnErrorArg proxy m p buffer (Maybe (SolutionError UserError))
doEditImport pi mi ii importStr = do
    case Import.parse importStr of
        Right newImport -> do
            lift $ removeImport pi mi ii
            _ <- lift $ addImport pi mi (WithBody newImport importStr)
            withGuiComponents $ lift . flip withSolutionTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doAddExport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> String
            -> DialogOnErrorArg proxy m p buffer (Maybe (SolutionError UserError))
doAddExport pi mi exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            _ <- lift $ addExport pi mi (WithBody newExport exportStr)
            withGuiComponents $ lift . flip withSolutionTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err

doRemoveExport :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> ExportId
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveExport pi mi ei = do
    lift $ removeExport pi mi ei
    withGuiComponents $ lift . flip withSolutionTree populateTree


doGetExport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetExport pi mi ei = do
    (WithBody _ b) <- lift $ getExport pi mi ei
    return $ Just b

doEditExport :: ( GuiCommand m p buffer )
             => ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> String
             -> DialogOnErrorArg proxy m p buffer (Maybe (SolutionError UserError))
doEditExport pi mi ei exportStr = do
    case Export.parse exportStr of
        Right newExport -> do
            lift $ removeExport pi mi ei
            _ <- lift $ addExport pi mi (WithBody newExport exportStr)
            withGuiComponents $ lift . flip withSolutionTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err
