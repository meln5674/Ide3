{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module MainSignals where

import Data.Text

import Data.Tree
import Data.Proxy
import Data.Functor.Compose

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding (withState)

import Control.Concurrent.MVar

import GI.Gtk hiding (TreePath, on, after)
import qualified GI.Gtk as Gtk
import GI.Gdk hiding (on, after)
import qualified GI.Gdk as Gdk

import Ide3.Types
import Ide3.Utils
import Ide3.Types.State
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Strict
import Ide3.Digest
import qualified Ide3.Solution as Solution

import GuiClass
import EnvironmentMonad

import GuiMonad
import GuiCommand
import GuiEnv

import Viewer

import ViewerMonad

import GuiViewer
import GuiViewer.Class

import ViewerMonad2

import GuiHelpers

import Dialogs.MainWindow (MainWindow)
import Dialogs.NewSolutionDialog (NewSolutionDialog)
import Dialogs.NewModuleDialog (NewModuleDialog)
import Dialogs.NewImportDialog (NewImportDialog)
import Dialogs.NewExportDialog (NewExportDialog)
import SolutionContextMenu (ContextMenu)

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.NewModuleDialog as NewModuleDialog
import qualified Dialogs.NewImportDialog as NewImportDialog
import qualified Dialogs.NewExportDialog as NewExportDialog
import qualified SolutionContextMenu

import SearchMode



import SolutionTree

import DeclarationPath

type MainGuiClass m' p m = 
    ( MonadIO m
    , MonadIO m'
    , ViewerMonad m'
    , PersistentSolutionMonad m'
    , ModuleLocationClass m'
    , MonadMask m
    , MonadMask m'
    , ErrorClass m'
    , GuiViewerClass m'
    , ViewerStateClass m'
    , InteruptMonad1 (MVar (MVarType p)) m'
    , EnvironmentMonad m'
    , Args (ProjectArgType m')
    , m' ~ ClassSolutionInitializerMonad (GuiEnvT m' p m')
    , Args (ArgType m')
    , SolutionInitializerClass (GuiEnvT m' p m')
    )

type MainGuiClassIO m p m' = ( MainGuiClass m p m' )

onNewSolutionConfirmed :: forall proxy m' p m
                       . ( MainGuiClassIO m' p m
                         )
                      => GuiEnvT {-proxy-} m' p m ()
onNewSolutionConfirmed = {-withNewSolutionDialog $ \dialog -> mapGuiEnv liftIO $ do
    projectRoot <- liftIO $ NewSolutionDialog.getSelectedFolder dialog
    projectName <- liftIO $ NewSolutionDialog.getSolutionName dialog
    templateName <- liftIO $ NewSolutionDialog.getTemplateName dialog
    doNew projectRoot projectName templateName
    liftIO $ NewSolutionDialog.close dialog-}
    doAddSolution
    

onNewClicked :: forall proxy m p m'
               . ( MainGuiClassIO m p m'
                 )
              => GuiEnvT {-proxy-} m p m' ()
onNewClicked = do
    {-env <- getEnv
    NewSolutionDialog.make $ \gui -> do
        liftIO $ gui `onGui` NewSolutionDialog.confirmClicked $ do
            liftIO $ runGuiEnvT (onNewSolutionConfirmed gui) env
            return False-}
    doNewStart
    return ()

onOpenClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p m' ()
onOpenClicked = do
    dialog <- Gtk.new FileChooserDialog
        [ #title := "Select solution file to open"
        , #action := FileChooserActionOpen
        ]
    dialogAddButton dialog "Close" $ fromIntegral $ fromEnum $ ResponseTypeReject
    dialogAddButton dialog "Open" $ fromIntegral $ fromEnum $ ResponseTypeAccept
    r <- liftM (toEnum . fromIntegral) $ dialogRun dialog
    case r of
        ResponseTypeAccept -> do
            Just path <- fileChooserGetFilename dialog
            widgetDestroy dialog
            doOpen path
            liftIO $ setCurrentDirectory $ takeDirectory path
        _ -> widgetDestroy dialog

onDigestClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p  m' ()
onDigestClicked = do
    dialog <- Gtk.new FileChooserDialog 
        [ #title := "Select directory to digest"
        , #action := FileChooserActionSelectFolder
        ]
    dialogAddButton dialog "Close" $ fromIntegral $ fromEnum ResponseTypeReject
    dialogAddButton dialog "Open" $ fromIntegral $ fromEnum ResponseTypeAccept
    r <- liftM (toEnum . fromIntegral) $ dialogRun dialog
    case r of
        ResponseTypeAccept -> do
            Just path <- fileChooserGetFilename dialog
            widgetDestroy dialog
            doOpen path
            liftIO $ setCurrentDirectory path
        _ -> widgetDestroy dialog



onDeclClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => Gtk.TreePath
              -> TreeViewColumn
              -> GuiEnvT {-proxy-} m p m' ()
onDeclClicked path _ = withGtkTreePath path doGetDecl

onBuildClicked :: forall proxy m p m'
               . ( MainGuiClassIO m p m' )
              => GuiEnvT {-proxy-} m p  m' ()
onBuildClicked = void $ mapGuiEnv liftIO $ doBuild

onRunClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p m' ()
onRunClicked = mapGuiEnv liftIO $ doRun


onSaveClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p m' ()
onSaveClicked = mapGuiEnv liftIO $ doSave

onSaveSolutionClicked :: forall proxy m p m' 
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p m' ()
onSaveSolutionClicked = mapGuiEnv liftIO $ doSaveSolution Nothing

onNewProjectClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => GuiEnvT {-proxy-} m p m'  Bool
onNewProjectClicked = undefined

onDeleteProjectClicked :: forall proxy m p m' 
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> GuiEnvT {-proxy-} m p m' Bool
onDeleteProjectClicked pi = undefined
    


onNewModuleClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> Maybe String
              -> GuiEnvT {-proxy-} m p m' Bool
onNewModuleClicked pi modName = do
    NewModuleDialog.make (fmap pack modName) $ \dialog -> do
        dialog `on1` NewModuleDialog.confirmClickedEvent $ \event -> do
            moduleName <- NewModuleDialog.getModuleName dialog
            case moduleName of
                "" -> doError $ InvalidOperation "Please enter a module name" ""
                name -> do
                    doAddModule pi (ModuleInfo (Symbol $ unpack moduleName))
                    NewModuleDialog.close dialog
            return False
        dialog `on1` NewModuleDialog.cancelClickedEvent $ \event -> do
            NewModuleDialog.close dialog
            return False
    return False

onNewImportClicked :: forall proxy m p m'
               . ( MainGuiClassIO m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT {-proxy-} m p m' Bool
onNewImportClicked pi mi = do
    NewImportDialog.makeNew $ \dialog -> do
        dialog `on1` NewImportDialog.confirmClickedEvent $ \event -> do
            import_ <- NewImportDialog.getImport dialog
            case import_ of
                "" -> doError $ InvalidOperation "Please enter an import" ""
                import_ -> do
                    maybeError <- doAddImport pi mi $ unpack import_
                    case maybeError of
                        Just err -> doError err
                        Nothing -> NewImportDialog.close dialog
            return False
        dialog `on1` NewImportDialog.cancelClickedEvent $ \event -> do
            NewImportDialog.close dialog
            return False
    return False


onEditImportClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> ImportId
              -> GuiEnvT {-proxy-} m p m' Bool
onEditImportClicked pi mi ii = do
    getResult <- doGetImport pi mi ii
    case getResult of
        Nothing -> return False
        Just importStr -> do
            NewImportDialog.makeEdit (pack importStr) $ \dialog -> do
                dialog `on1` NewImportDialog.confirmClickedEvent $ \event -> do
                    import_ <- NewImportDialog.getImport dialog
                    case import_ of
                        "" -> doError $ InvalidOperation "Please enter an import" ""
                        import_ -> do
                            maybeError <- doEditImport pi mi ii $ unpack import_
                            case maybeError of
                                Just err -> doError err
                                Nothing -> NewImportDialog.close dialog
                    return False
                dialog `on1` NewImportDialog.cancelClickedEvent $ \event -> do
                    NewImportDialog.close dialog
                    return False
            return False

onNewExportClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT {-proxy-} m p m' Bool
onNewExportClicked pi mi = do
    NewExportDialog.makeNew $ \dialog -> do
        dialog `on1` NewExportDialog.confirmClickedEvent $ \event -> do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- doAddExport pi mi $ unpack export_
                    case maybeError of
                        Just err -> mapGuiEnv liftIO $ doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `on1` NewExportDialog.cancelClickedEvent $ \event -> do
            NewExportDialog.close dialog
            return False
    return False

onExportAllClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT {-proxy-} m p m' Bool
onExportAllClicked pi mi = mapGuiEnv liftIO $ do
    doExportAll pi mi
    return False
    

onEditExportClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> ExportId
              -> GuiEnvT {-proxy-} m p m' Bool
onEditExportClicked pi mi ii = do
    getResult <- doGetExport pi mi ii
    case getResult of
        Nothing -> return False
        Just exportStr -> do
            NewExportDialog.makeEdit (pack exportStr) $ \dialog -> do
                dialog `on1` NewExportDialog.confirmClickedEvent $ \event -> do
                    export_ <- NewExportDialog.getExport dialog
                    case export_ of
                        "" -> doError $ InvalidOperation "Please enter an export" ""
                        export_ -> do
                            maybeError <- doEditExport pi mi ii $ unpack export_
                            case maybeError of
                                Just err -> doError err
                                Nothing -> NewExportDialog.close dialog
                    return False
                dialog `on1` NewExportDialog.cancelClickedEvent $ \event -> do
                    NewExportDialog.close dialog
                    return False
            return False

onExportDeclarationClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => ProjectInfo
              -> ModuleInfo
              -> DeclarationInfo
              -> GuiEnvT {-proxy-} m p m' Bool
onExportDeclarationClicked pi mi (DeclarationInfo (Symbol declStr)) = do
    NewExportDialog.makeEdit (pack declStr) $ \dialog -> do
        dialog `on1` NewExportDialog.confirmClickedEvent $ \event -> do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- doAddExport pi mi $ unpack export_
                    case maybeError of
                        Just err -> doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `on1` NewExportDialog.cancelClickedEvent $ \event -> do
            NewExportDialog.close dialog
            return False
    return False


setupModuleContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupModuleContextMenu pi mi = do
    menu <- SolutionContextMenu.makeModuleMenu pi mi
    menu `on1` SolutionContextMenu.newSubModuleClickedEvent $ \event -> do
        onNewModuleClicked pi $ case mi of
            mi@(ModuleInfo (Symbol prefix)) -> Just prefix
            mi -> Nothing
    menu `on1` SolutionContextMenu.newDeclClickedEvent $ \event -> do
        doAddDeclaration pi mi $ DeclarationInfo $ Symbol "New Declaration"
        return False
    menu `on1` SolutionContextMenu.deleteModuleClickedEvent $ \event -> do
        doRemoveModule pi mi
        return False
    return menu


setupProjectContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupProjectContextMenu pi = do
    menu <- SolutionContextMenu.makeProjectMenu pi
    menu `on1` SolutionContextMenu.newModuleClickedEvent $ \event -> do
        onNewModuleClicked pi Nothing
    menu `on1` SolutionContextMenu.deleteProjectClickedEvent $ \event -> do
        onDeleteProjectClicked pi
    return menu

setupSolutionContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => GuiEnvT {-proxy-} m p  m' ContextMenu
setupSolutionContextMenu = do
    menu <- SolutionContextMenu.makeSolutionMenu
    menu `on1` SolutionContextMenu.newProjectClickedEvent $ \event -> do
        onNewProjectClicked
    return menu

setupDeclContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupDeclContextMenu pi mi di = do
    menu <- SolutionContextMenu.makeDeclMenu pi mi di
    menu `on1` SolutionContextMenu.deleteDeclarationClickedEvent $ \event -> do
        doRemoveDeclaration pi mi di
        return False
    menu `on1` SolutionContextMenu.exportDeclarationClickedEvent $ \event -> do
        onExportDeclarationClicked pi mi di
    menu `on1` SolutionContextMenu.unExportDeclarationClickedEvent $ \event -> do
        doUnExportDeclaration pi mi di
        return False
    return menu

setupImportsContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupImportsContextMenu pi mi = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeImportsMenu pi mi
    menu `on1` SolutionContextMenu.newImportClickedEvent $ \event -> onNewImportClicked pi mi
    return menu


setupExportsContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupExportsContextMenu pi mi = do
    menu <- SolutionContextMenu.makeExportsMenu pi mi
    menu `on1` SolutionContextMenu.newExportClickedEvent $ \event -> onNewExportClicked pi mi
    menu `on1` SolutionContextMenu.exportAllClickedEvent $ \event -> onExportAllClicked pi mi
    return menu

setupImportContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ImportId
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupImportContextMenu pi mi ii = do
    menu <- SolutionContextMenu.makeImportMenu pi mi ii
    menu `on1` SolutionContextMenu.deleteImportClickedEvent $ \event -> do
        doRemoveImport pi mi ii
        return False
    menu `on1` SolutionContextMenu.editImportClickedEvent $ \event -> do
        onEditImportClicked pi mi ii
        return False
    return menu

setupExportContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ExportId
                 -> GuiEnvT {-proxy-} m p  m' ContextMenu
setupExportContextMenu pi mi ei = do
    menu <- SolutionContextMenu.makeExportMenu pi mi ei
    menu `on1` SolutionContextMenu.deleteExportClickedEvent $ \event -> do
        doRemoveExport pi mi ei
        return False
    menu `on1` SolutionContextMenu.editExportClickedEvent $ \event -> do
        onEditExportClicked pi mi ei
        return False
    return menu                        

onSolutionViewClicked :: forall proxy m p m'
               . ( MainGuiClassIO m p m' )
              => MainWindow
              -> EventButton -> GuiEnvT {-proxy-} m p m' Bool
onSolutionViewClicked gui event = do
    button <- Gdk.get event #button
    when (button == fromIntegral BUTTON_SECONDARY) $ do
        x <- Gtk.get event #x
        y <- Gtk.get event #y
        time <- Gtk.get event #time
        let (x',y') = (round x, round y)
        pathClicked <- MainWindow.getSolutionPathClicked (x',y') gui
        menu <- case pathClicked of
            Nothing -> setupSolutionContextMenu
            Just (path, col, p) -> withGuiComponents $ \comp -> do
                item <- findAtPath path
                case item of
                    ProjectResult pi -> setupProjectContextMenu pi
                    ModuleResult pi mi _ -> setupModuleContextMenu pi mi
                    DeclResult pi mi di -> setupDeclContextMenu pi mi di
                    ImportsResult pi mi -> setupImportsContextMenu pi mi
                    ExportsResult pi mi -> setupExportsContextMenu pi mi
                    ImportResult pi mi ii -> setupImportContextMenu pi mi ii
                    ExportResult pi mi ei -> setupExportContextMenu pi mi ei
                    NoSearchResult -> setupSolutionContextMenu
        lift $ SolutionContextMenu.showMenu menu event
    return False    


onDeclEdited :: forall proxy m p m'
               . ( MainGuiClassIO m p m' )
              => GuiEnvT {-proxy-} m p m' ()
onDeclEdited = reapplySyntaxHighlighting

{-
onFindClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => MainWindow
              -> GuiEnvT {-proxy-} m p  IO ()
onFindClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Find
    doSetSearchMode Find

onNavigateClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => MainWindow
              -> GuiEnvT {-proxy-} m p  IO ()
onNavigateClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Navigate
    doSetSearchMode Navigate

onSearchClicked :: forall proxy m p  m'
               . ( MainGuiClass m p  )
              => GuiEnvT {-proxy-} m p  IO ()
onSearchClicked = doSearch
-}

onGotoDeclarationClicked
    :: forall proxy m p m'
     . ( MainGuiClassIO m p m' )
    => GuiEnvT {-proxy-} m p m' ()
onGotoDeclarationClicked = doGotoDeclaration

onBackClicked
    :: forall proxy m p m'
     . ( MainGuiClassIO m p m' )
    => GuiEnvT {-proxy-} m p m' ()
onBackClicked = doBackHistory

onForwardClicked
    :: forall proxy m' p m
     . ( MainGuiClassIO m' p m )
    => GuiEnvT {-proxy-} m' p m ()
onForwardClicked = doForwardHistory

onErrorClicked :: forall proxy m p m'
               . ( MainGuiClass m p m' )
              => MainWindow
              -> Gtk.TreePath
              -> TreeViewColumn
              -> GuiEnvT {-proxy-} m p m' ()
onErrorClicked gui path _ = do
    shouldFocus <- withGtkTreePath path doJumpToErrorLocation
    when shouldFocus $ MainWindow.focusDeclView gui
        

{-
declBufferEdited :: GuiEnvSignal proxy m' p IO GuiComponents TextBuffer IO ()
declBufferEdited = mkGuiEnvSignal (flip withEditorBuffer id) endUserAction
-}

declBufferEdited :: SubSignalProxy GuiComponents TextBuffer TextBufferEndUserActionSignalInfo
declBufferEdited comp = withEditorBuffer comp $ \buffer -> (buffer, #endUserAction)

setupSignals :: ( MainGuiClassIO m p m') => MainWindow -> GuiEnvT m p m' ()
setupSignals gui = do
    gui `on` MainWindow.newClickedEvent $ onNewClicked
    gui `on` MainWindow.openClickedEvent $ onOpenClicked
    gui `on` MainWindow.digestClickedEvent $ onDigestClicked
    gui `on` MainWindow.saveClickedEvent $ onSaveClicked
    gui `on` MainWindow.saveSolutionClickedEvent $ onSaveSolutionClicked
    gui `on2` MainWindow.declClickedEvent $ onDeclClicked
    gui `on1` MainWindow.projectViewClickedEvent $ onSolutionViewClicked gui
    gui `on` MainWindow.buildClickedEvent $ onBuildClicked
    gui `on` MainWindow.runClickedEvent $ onRunClicked
    --gui `on` MainWindow.findClickedEvent $ onFindClicked gui
    --gui `on` MainWindow.navigateClickedEvent $ onNavigateClicked gui
    --gui `on` MainWindow.searchClickedEvent $ onSearchClicked
    gui `on` MainWindow.gotoDeclarationClickedEvent $ onGotoDeclarationClicked
    gui `on` MainWindow.backClickedEvent $ onBackClicked
    gui `on` MainWindow.forwardClickedEvent $ onForwardClicked
    gui `on2` MainWindow.errorClickedEvent $ onErrorClicked gui
    gui `on` MainWindow.windowClosedEvent $ liftIO exitSuccess
    withGuiComponents $ \comp  -> do
        comp `after` declBufferEdited $ onDeclEdited
    return ()

