{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.Trans.Identity
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
--import GuiCommand
import GuiCommandGeneric
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

import GuiT
import Dialogs
import Dialogs.Class

import SolutionTree

import DeclarationPath

type MainGuiClass t m' p m = 
    ( MonadIO m
    , MonadIO m'
    , MonadIO (t m)
    , MonadIO (t m)
    , MonadIO (t IO)
    , ViewerMonad m'
    , PersistentSolutionMonad m'
    , ModuleLocationClass m'
    , MonadMask m
    , MonadMask m'
    , ErrorClass m'
    , ViewerStateClass m'
--    , InteruptMonad1 (MVar (MVarType p)) m'
    , EnvironmentMonad m'
    , Args (ProjectArgType m')
    , m' ~ ClassSolutionInitializerMonad (t m')
    , Args (ArgType m')
    , SolutionInitializerClass (t m')
    , GuiCommand2 t m' m
    , GuiCommand2 t m' IO
    , SignalInterceptClass t
    , SolutionViewClass (t IO)
    , EditorBufferClass (t IO)
    , SolutionViewClass (t m)
    , EditorBufferClass (t m)
    )

type MainGuiClassIO t m' p m = ( MainGuiClass t m' p m )

onNewSolutionConfirmed :: forall t proxy m' p m
                       . ( MainGuiClassIO t m' p m
                         )
                      => t m ()
onNewSolutionConfirmed = {-withNewSolutionDialog $ \dialog -> id $ do
    projectRoot <- liftIO $ NewSolutionDialog.getSelectedFolder dialog
    projectName <- liftIO $ NewSolutionDialog.getSolutionName dialog
    templateName <- liftIO $ NewSolutionDialog.getTemplateName dialog
    doNew projectRoot projectName templateName
    liftIO $ NewSolutionDialog.close dialog-}
    doAddSolution
    

onNewClicked :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m
                 )
              => t m ()
onNewClicked = do
    {-env <- getEnv
    NewSolutionDialog.make $ \gui -> do
        liftIO $ gui `onGui` NewSolutionDialog.confirmClicked $ do
            liftIO $ runGuiEnvT (onNewSolutionConfirmed gui) env
            return False-}
    doNewStart
    return ()

onOpenClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
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

onDigestClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
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



onDeclClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => Gtk.TreePath
              -> TreeViewColumn
              -> t m ()
onDeclClicked path _ = withGtkTreePath path doGetDecl

onBuildClicked :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
              => t m ()
onBuildClicked = void $ doBuild

onRunClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onRunClicked = doRun


onSaveClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onSaveClicked = doSave

onSaveSolutionClicked :: forall t proxy m' p m 
               . ( MainGuiClass t m' p m )
              => t m ()
onSaveSolutionClicked = id $ doSaveSolution Nothing

onNewProjectClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => t m  Bool
onNewProjectClicked = undefined

onDeleteProjectClicked :: forall t proxy m' p m 
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> t m Bool
onDeleteProjectClicked pi = undefined
    


onNewModuleClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> Maybe String
              -> t m Bool
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

onNewImportClicked :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
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


onEditImportClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> ImportId
              -> t m Bool
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

onNewExportClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
onNewExportClicked pi mi = do
    NewExportDialog.makeNew $ \dialog -> do
        dialog `on1` NewExportDialog.confirmClickedEvent $ \event -> do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- doAddExport pi mi $ unpack export_
                    case maybeError of
                        Just err -> id $ doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `on1` NewExportDialog.cancelClickedEvent $ \event -> do
            NewExportDialog.close dialog
            return False
    return False

onExportAllClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
onExportAllClicked pi mi = id $ do
    doExportAll pi mi
    return False
    

onEditExportClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> ExportId
              -> t m Bool
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

onExportDeclarationClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> DeclarationInfo
              -> t m Bool
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


setupModuleContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
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


setupProjectContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> t m ContextMenu
setupProjectContextMenu pi = do
    menu <- SolutionContextMenu.makeProjectMenu pi
    menu `on1` SolutionContextMenu.newModuleClickedEvent $ \event -> do
        onNewModuleClicked pi Nothing
    menu `on1` SolutionContextMenu.deleteProjectClickedEvent $ \event -> do
        onDeleteProjectClicked pi
    return menu

setupSolutionContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => t m ContextMenu
setupSolutionContextMenu = do
    menu <- SolutionContextMenu.makeSolutionMenu
    menu `on1` SolutionContextMenu.newProjectClickedEvent $ \event -> do
        onNewProjectClicked
    return menu

setupDeclContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t m ContextMenu
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

setupImportsContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
setupImportsContextMenu pi mi = id $ do
    menu <- SolutionContextMenu.makeImportsMenu pi mi
    menu `on1` SolutionContextMenu.newImportClickedEvent $ \event -> onNewImportClicked pi mi
    return menu


setupExportsContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
setupExportsContextMenu pi mi = do
    menu <- SolutionContextMenu.makeExportsMenu pi mi
    menu `on1` SolutionContextMenu.newExportClickedEvent $ \event -> onNewExportClicked pi mi
    menu `on1` SolutionContextMenu.exportAllClickedEvent $ \event -> onExportAllClicked pi mi
    return menu

setupImportContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ImportId
                 -> t m ContextMenu
setupImportContextMenu pi mi ii = do
    menu <- SolutionContextMenu.makeImportMenu pi mi ii
    menu `on1` SolutionContextMenu.deleteImportClickedEvent $ \event -> do
        doRemoveImport pi mi ii
        return False
    menu `on1` SolutionContextMenu.editImportClickedEvent $ \event -> do
        onEditImportClicked pi mi ii
        return False
    return menu

setupExportContextMenu :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ExportId
                 -> t m ContextMenu
setupExportContextMenu pi mi ei = do
    menu <- SolutionContextMenu.makeExportMenu pi mi ei
    menu `on1` SolutionContextMenu.deleteExportClickedEvent $ \event -> do
        doRemoveExport pi mi ei
        return False
    menu `on1` SolutionContextMenu.editExportClickedEvent $ \event -> do
        onEditExportClicked pi mi ei
        return False
    return menu                        

onSolutionViewClicked :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m )
              => MainWindow
              -> EventButton -> t m Bool
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
            Just (path, col, p) -> do
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


onDeclEdited :: forall t proxy m' p m
               . ( MainGuiClassIO t m' p m 
                 , EditorBufferClass (t m)
                 )
              => t m ()
onDeclEdited = reapplySyntaxHighlighting

{-
onFindClicked :: forall t proxy m p 
               . ( MainGuiClass t m p  )
              => MainWindow
              -> t  IO ()
onFindClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Find
    doSetSearchMode Find

onNavigateClicked :: forall t proxy m p 
               . ( MainGuiClass t m p  )
              => MainWindow
              -> t  IO ()
onNavigateClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Navigate
    doSetSearchMode Navigate

onSearchClicked :: forall t proxy m' p m
               . ( MainGuiClass t m p  )
              => t  IO ()
onSearchClicked = doSearch
-}

onGotoDeclarationClicked
    :: forall t proxy m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onGotoDeclarationClicked = doGotoDeclaration

onBackClicked
    :: forall t proxy m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onBackClicked = doBackHistory

onForwardClicked
    :: forall t proxy m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onForwardClicked = doForwardHistory

onErrorClicked :: forall t proxy m' p m
               . ( MainGuiClass t m' p m )
              => MainWindow
              -> Gtk.TreePath
              -> TreeViewColumn
              -> t m ()
onErrorClicked gui path _ = do
    shouldFocus <- withGtkTreePath path doJumpToErrorLocation
    when shouldFocus $ MainWindow.focusDeclView gui
        

{-
declBufferEdited :: GuiEnvSignal proxy m' p IO GuiComponents TextBuffer IO ()
declBufferEdited = mkGuiEnvSignal (flip withEditorBuffer id) endUserAction
-}

declBufferEdited :: SubSignalProxy GuiComponents TextBuffer TextBufferEndUserActionSignalInfo
declBufferEdited comp = withEditorBuffer comp $ \buffer -> (buffer, #endUserAction)

setupSignals :: ( MainGuiClassIO t m' p m ) => MainWindow -> t m ()
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
    gui `on1` MainWindow.declarationEditedEvent $ \_ -> onDeclEdited
    gui `on2` MainWindow.errorClickedEvent $ onErrorClicked gui
    gui `on` MainWindow.windowClosedEvent $ liftIO exitSuccess
    return ()


setupKeyboardShortcuts :: MonadIO m => MainWindow -> AccelGroup -> m ()
setupKeyboardShortcuts gui group = do
    gui `MainWindow.addAccelGroup` group
    MainWindow.addNewClickedEventAccelerator gui group
        KEY_n [ModifierTypeControlMask, ModifierTypeShiftMask] [AccelFlagsVisible]
    MainWindow.addOpenClickedEventAccelerator gui group
        KEY_o [ModifierTypeControlMask] [AccelFlagsVisible]
    MainWindow.addDigestClickedEventAccelerator gui group
        KEY_o [ModifierTypeControlMask, ModifierTypeShiftMask] [AccelFlagsVisible]
    MainWindow.addSaveClickedEventAccelerator gui group
        KEY_s [ModifierTypeControlMask] [AccelFlagsVisible]
    MainWindow.addSaveSolutionClickedEventAccelerator gui group
        KEY_s [ModifierTypeControlMask,ModifierTypeShiftMask] [AccelFlagsVisible]
    MainWindow.addBuildClickedEventAccelerator gui group
        KEY_F5 [] [AccelFlagsVisible]
    {-MainWindow.addFindClickedEventAccelerator gui group
        KEY_f [ModifierTypeControlMask] [AccelFlagsVisible]-}
    {-MainWindow.addNavigateClickedEventAccelerator gui group
        KEY_KP_Space [ModifierTypeControlMask] [AccelFlagsVisible]-}
    MainWindow.addGotoDeclarationEventAccelerator gui group
        KEY_d [ModifierTypeControlMask] [AccelFlagsVisible]
    MainWindow.addBackEventAccelerator gui group
        KEY_less [ModifierTypeControlMask] [AccelFlagsVisible]
    MainWindow.addForwardEventAccelerator gui group
        KEY_greater [ModifierTypeControlMask] [AccelFlagsVisible]
    
