{-# LANGUAGE ConstraintKinds #-}
module GuiCommand.Internal where

import System.Directory
import System.FilePath

import Graphics.UI.Gtk

import Control.Monad.Catch

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict (gets)

import Ide3.Types
import Ide3.Utils
import Ide3.NewMonad

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
      , SolutionClass m
      , PersistenceClass m
      , ProjectModuleClass m
      , ProjectExternModuleClass m
      , ModuleExportClass m
      , ModuleImportClass m
      , ModuleDeclarationClass m
      , ModulePragmaClass m
      , ExternModuleExportClass m
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
        {- -- Why is this here?
                bounce $ createNewFile $ projectName ++ ".proj"
        -}
            lift $ bounce $ setDirectoryToOpen $ projectRoot </> projectName
            r <- lift 
                    $ ExceptT 
                    $ lift 
                    $ runExceptT 
                    $ runInitializer stackInitializer 
                                    (StackInitializerArgs projectName templateName)
            case r of
                InitializerSucceeded{} -> do
                    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
                    lift $ saveSolution $ Just $ projectRoot </> projectName
                InitializerFailed out err -> lift $ throwE $ InvalidOperation (out ++ err) ""

doOpen :: ( GuiCommand m p buffer )
       => FilePath
       -> DialogOnErrorArg proxy m p buffer ()
doOpen path = do
    lift $ openSolution path
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree


doGetDecl :: ( GuiCommand m p buffer )
          => TreePath
          -> TreeViewColumn 
          -> DialogOnErrorArg proxy m p buffer ()
doGetDecl path _ = withGuiComponents $ \comp -> do
    index <- lift $ wrapIOError $ withSolutionTree comp $ findAtPath path
    case index of
        DeclResult pi mi di -> do
                decl <- lift $ bounce $ getDeclaration pi mi di
                lift $ wrapIOError $ comp `setDeclBufferText` body decl
                lift $ lift $ setCurrentDecl pi mi di
        ModuleResult pi mi True -> do
            header <- lift $ bounce $ getModuleHeader pi mi
            lift $ wrapIOError $ comp `setDeclBufferText` header
            lift $ lift $ setCurrentModule pi mi
        _ -> return ()

doBuild :: ( GuiCommand m p buffer
           , MonadMask m
           )
        => DialogOnErrorArg proxy m p buffer ()
doBuild = withGuiComponents $ \comp -> lift $ do
    bounce $ prepareBuild
    r <- bounce $ runBuilder stackBuilder
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
    let doWithBuffer f = do
            text <- wrapIOError $ withEditorBuffer comp $ \buffer -> do
                start <- textBufferGetStartIter buffer
                end <- textBufferGetEndIter buffer
                textBufferGetText buffer start end False
            result <- f text
            saveSolution Nothing
            return result
    declResult <- lift $ getCurrentDeclaration
    modResult <- lift $ getCurrentModule
    case declResult of
        Just (pi, mi, di) -> do
            di' <- doWithBuffer $ \text -> do
                bounce $ editDeclaration pi mi di $ const $ Declaration.parseAndCombine text Nothing
            bounce $ withSolutionTree comp populateTree
            when (di /= di') $ do
                lift $ setCurrentDecl pi mi di'
        _ -> case modResult of
            Just (pi, mi) -> doWithBuffer $ \text ->
                bounce $ editModuleHeader pi mi $ const text
            _ -> return ()
        

doSaveSolution :: ( GuiCommand m p buffer
                 , MonadMask m
                 )
              => Maybe FilePath
              -> DialogOnErrorArg proxy m p buffer ()
doSaveSolution = lift . saveSolution

doAddModule :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> DialogOnErrorArg proxy m p buffer ()
doAddModule pi mi = do
    lift $ bounce $ createModule pi mi
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree

doRemoveModule :: ( GuiCommand m p buffer )
               => ProjectInfo
               -> ModuleInfo
               -> DialogOnErrorArg proxy m p buffer ()
doRemoveModule pi mi = do
    lift $ bounce $ removeModule pi mi
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree


doAddDeclaration :: ( GuiCommand m p buffer )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> DialogOnErrorArg proxy m p buffer ()
doAddDeclaration pi mi di = do
    let newdecl = WithBody (UnparseableDeclaration di) ""
    lift $ bounce $ addDeclaration pi mi newdecl
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree

doRemoveDeclaration :: ( GuiCommand m p buffer )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doRemoveDeclaration pi mi di = do
    lift $ bounce $ removeDeclaration pi mi di
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree

doUnExportDeclaration :: ( GuiCommand m p buffer )
                    => ProjectInfo
                    -> ModuleInfo
                    -> DeclarationInfo
                    -> DialogOnErrorArg proxy m p buffer ()
doUnExportDeclaration pi mi (DeclarationInfo sym) = do
    matches <- lift $ bounce $ do
        es <- do
            --m <- getModule pi mi
            maybeEis <- getExports pi mi
            case maybeEis of
                Nothing -> throwE $ InvalidOperation "All symbols are exported" ""
                Just eis -> 
                    forM eis $ \ei -> do
                        (WithBody e _) <- getExport pi mi ei
                        syms <- Export.symbolsProvided' pi mi e
                        return (ei,syms)
        return $ flip filter es $ \(_,syms) -> sym `elem` syms
    case matches of
        [] -> lift $ throwE $ SymbolNotExported mi sym ""
        [(ei,syms)] -> do
            case syms of
                [] -> lift $ throwE $ InvalidOperation "Internal Error" "doUnExportDeclaration"
                [_] -> do
                    lift $ bounce $ removeExport pi mi ei
                    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
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
            _ <- lift $ bounce $ addImport pi mi (WithBody newImport importStr)
            withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
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
    lift $ bounce $ removeImport pi mi ii
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree


doGetImport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ImportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetImport pi mi ii = do
    (WithBody _ b) <- lift $ bounce $ getImport pi mi ii
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
            lift $ bounce $ removeImport pi mi ii
            _ <- lift $ bounce $ addImport pi mi (WithBody newImport importStr)
            withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
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
            _ <- lift $ bounce $ addExport pi mi (WithBody newExport exportStr)
            withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
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
    lift $ bounce $ removeExport pi mi ei
    withGuiComponents $ lift . bounce . flip withSolutionTree populateTree


doGetExport :: ( GuiCommand m p buffer )
            => ProjectInfo
            -> ModuleInfo
            -> ExportId
            -> DialogOnErrorArg proxy m p buffer (Maybe String)
doGetExport pi mi ei = do
    (WithBody _ b) <- lift $ bounce $ getExport pi mi ei
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
            lift $ bounce $ removeExport pi mi ei
            _ <- lift $ bounce $ addExport pi mi (WithBody newExport exportStr)
            withGuiComponents $ lift . bounce . flip withSolutionTree populateTree
            return Nothing
        Left parseError -> case parseError of
            err@ParseError{} -> return $ Just err
            err -> lift $ throwE err
