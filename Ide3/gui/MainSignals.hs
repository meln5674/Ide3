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

import Data.Int
import Data.Text

import System.Exit
import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans

import GI.Gtk hiding (TreePath, on, after)
import qualified GI.Gtk as Gtk
import GI.Gdk hiding (on, after)
import qualified GI.Gdk as Gdk

import Ide3.Types
import Ide3.NewMonad

import GuiClass
import EnvironmentMonad

import GuiMonad
import GuiCommandGeneric

import Viewer

import GuiHelpers

import Dialogs.MainWindow (MainWindow, FocusTarget (..), SolutionPathCoords (..))
import SolutionContextMenu (ContextMenu)

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewModuleDialog as NewModuleDialog
import qualified Dialogs.NewImportDialog as NewImportDialog
import qualified Dialogs.NewExportDialog as NewExportDialog
import qualified Dialogs.MoveDeclarationDialog as MoveDeclarationDialog
import qualified SolutionContextMenu

import GenericGuiEnv

import DeclarationPath

import SearchMode

import SolutionTree

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
    , m' ~ ClassProjectInitializerMonad (t m')
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

onNewSolutionConfirmed :: forall t {-proxy-} m' p m
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
    

onNewProjectConfirmed :: forall t {-proxy-} m' p m
                       . ( MainGuiClassIO t m' p m
                         )
                      => t m ()
onNewProjectConfirmed = doAddProject

onEditProjectConfirmed :: forall t {-proxy-} m' p m
                       . ( MainGuiClassIO t m' p m
                         )
                      => ProjectInfo
                      -> t m ()
onEditProjectConfirmed = doEditProject

onNewClicked :: forall t {-proxy-} m' p m
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

onOpenClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onOpenClicked = do
    dialog <- Gtk.new FileChooserDialog
        [ #title := "Select solution file to open"
        , #action := FileChooserActionOpen
        ]
    void $ dialogAddButton dialog "Close" 
         $ fromIntegral 
         $ fromEnum 
         $ ResponseTypeReject
    void $ dialogAddButton dialog "Open"
         $ fromIntegral 
         $ fromEnum 
         $ ResponseTypeAccept
    result <- liftM (toEnum . fromIntegral) $ dialogRun dialog
    case result of
        ResponseTypeAccept -> do
            Just path <- fileChooserGetFilename dialog
            widgetDestroy dialog
            doOpen path
            liftIO $ setCurrentDirectory $ takeDirectory path
        _ -> widgetDestroy dialog

onDigestClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onDigestClicked = do
    dialog <- Gtk.new FileChooserDialog 
        [ #title := "Select directory to digest"
        , #action := FileChooserActionSelectFolder
        ]
    void $ dialogAddButton dialog "Close" 
         $ fromIntegral 
         $ fromEnum 
         $ ResponseTypeReject
    void $ dialogAddButton dialog "Open" 
         $ fromIntegral 
         $ fromEnum 
         $ ResponseTypeAccept
    result <- liftM (toEnum . fromIntegral) $ dialogRun dialog
    case result of
        ResponseTypeAccept -> do
            Just path <- fileChooserGetFilename dialog
            widgetDestroy dialog
            doOpen path
            liftIO $ setCurrentDirectory path
        _ -> widgetDestroy dialog



onDeclClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => Gtk.TreePath
              -> TreeViewColumn
              -> t m ()
onDeclClicked path _ = withGtkTreePath path doGetDecl

onBuildClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
              => t m ()
onBuildClicked = void $ doBuild

onRunClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onRunClicked = doRun


onSaveClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onSaveClicked = doSave

onSaveSolutionClicked :: forall t {-proxy-} m' p m 
               . ( MainGuiClass t m' p m )
              => t m ()
onSaveSolutionClicked = id $ doSaveSolution Nothing

onNewProjectClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => t m ()
onNewProjectClicked = doNewProjectStart

onEditProjectClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> t m ()
onEditProjectClicked = doEditProjectStart

onDeleteProjectClicked :: forall t {-proxy-} m' p m 
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> t m Bool
onDeleteProjectClicked pji = undefined

onNewModuleClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> Maybe Text
              -> t m Bool
onNewModuleClicked pji modName = do
    NewModuleDialog.make modName $ \dialog -> do
        dialog `on` NewModuleDialog.confirmClickedEvent $ Func1 $ \event -> do
            moduleName <- NewModuleDialog.getModuleName dialog
            case moduleName of
                "" -> doError $ InvalidOperation "Please enter a module name" ""
                name -> do
                    doAddModule pji (ModuleInfo (Symbol moduleName))
                    NewModuleDialog.close dialog
            return False
        dialog `on` NewModuleDialog.cancelClickedEvent $ Func1 $ \event -> do
            NewModuleDialog.close dialog
            return False
    return False

onNewImportClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
onNewImportClicked pji mi = do
    NewImportDialog.makeNew $ \dialog -> do
        dialog `on` NewImportDialog.confirmClickedEvent $ Func1 $ \event -> do
            import_ <- NewImportDialog.getImport dialog
            case import_ of
                "" -> doError $ InvalidOperation "Please enter an import" ""
                import_ -> do
                    maybeError <- doAddImport pji mi import_
                    case maybeError of
                        Just err -> doError err
                        Nothing -> NewImportDialog.close dialog
            return False
        dialog `on` NewImportDialog.cancelClickedEvent $ Func1 $ \event -> do
            NewImportDialog.close dialog
            return False
    return False


onEditImportClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> ImportId
              -> t m Bool
onEditImportClicked pji mi ii = do
    getResult <- doGetImport pji mi ii
    case getResult of
        Nothing -> return False
        Just importStr -> do
            NewImportDialog.makeEdit importStr $ \dialog -> do
                dialog `on` NewImportDialog.confirmClickedEvent $ Func1 $ \event -> do
                    import_ <- NewImportDialog.getImport dialog
                    case import_ of
                        "" -> doError $ InvalidOperation "Please enter an import" ""
                        import_ -> do
                            maybeError <- doEditImport pji mi ii import_
                            case maybeError of
                                Just err -> doError err
                                Nothing -> NewImportDialog.close dialog
                    return False
                dialog `on` NewImportDialog.cancelClickedEvent $ Func1 $ \event -> do
                    NewImportDialog.close dialog
                    return False
            return False

onNewExportClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
onNewExportClicked pji mi = do
    NewExportDialog.makeNew $ \dialog -> do
        dialog `on` NewExportDialog.confirmClickedEvent $ Func1 $ \event -> do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- doAddExport pji mi export_
                    case maybeError of
                        Just err -> id $ doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `on` NewExportDialog.cancelClickedEvent $ Func1 $ \event -> do
            NewExportDialog.close dialog
            return False
    return False

onExportAllClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> t m Bool
onExportAllClicked pji mi = id $ do
    doExportAll pji mi
    return False
    

onEditExportClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> ExportId
              -> t m Bool
onEditExportClicked pji mi ii = do
    getResult <- doGetExport pji mi ii
    case getResult of
        Nothing -> return False
        Just exportStr -> do
            NewExportDialog.makeEdit exportStr $ \dialog -> do
                dialog `on` NewExportDialog.confirmClickedEvent $ Func1 $ \event -> do
                    export_ <- NewExportDialog.getExport dialog
                    case export_ of
                        "" -> doError $ InvalidOperation "Please enter an export" ""
                        export_ -> do
                            maybeError <- doEditExport pji mi ii export_
                            case maybeError of
                                Just err -> doError err
                                Nothing -> NewExportDialog.close dialog
                    return False
                dialog `on` NewExportDialog.cancelClickedEvent $ Func1 $ \event -> do
                    NewExportDialog.close dialog
                    return False
            return False

onExportDeclarationClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => ProjectInfo
              -> ModuleInfo
              -> DeclarationInfo
              -> t m Bool
onExportDeclarationClicked pji mi (SymbolDeclarationInfo (Symbol declStr)) = do
    NewExportDialog.makeEdit declStr $ \dialog -> do
        dialog `on` NewExportDialog.confirmClickedEvent $ Func1 $ \event -> do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- doAddExport pji mi export_
                    case maybeError of
                        Just err -> doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `on` NewExportDialog.cancelClickedEvent $ Func1 $ \event -> do
            NewExportDialog.close dialog
            return False
    return False
onExportDeclarationClicked pji mi _ = do
    doError $ InvalidOperation "This declaration is not exportable" ""
    return False
    

onMoveDeclarationClicked :: forall t m' p m
                          . ( MainGuiClass t m' p m )
                         => ProjectInfo
                         -> ModuleInfo
                         -> DeclarationInfo
                         -> t m Bool
onMoveDeclarationClicked pji mi di = do
    MoveDeclarationDialog.make $ \dialog -> do
        dialog `on` MoveDeclarationDialog.confirmClickedEvent $ Func1 $ \event -> do
            pathClicked <- MoveDeclarationDialog.getSelectedModulePath dialog
            case pathClicked of
                Nothing -> doError $ InvalidOperation "Please select a module" 
                                                      ""
                Just (path, col) -> do
                    item <- findAtPath path
                    case item of
                        ModuleResult pji' mi' _ -> do
                            doMoveDeclaration pji mi di pji' mi'
                            MoveDeclarationDialog.close dialog
                        ProjectResult _ -> do
                            doError $ InvalidOperation "Please select a module"
                                                       ""
                        NoSearchResult -> do
                            doError $ InvalidOperation "An internal error has occured"
                                                       ""
            return False
        dialog `on` MoveDeclarationDialog.cancelClickedEvent $ Func1 $ \event -> do
            MoveDeclarationDialog.close dialog
            return False
    return False


setupModuleContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
setupModuleContextMenu pji mi = do
    menu <- SolutionContextMenu.makeModuleMenu pji mi
    menu `on` SolutionContextMenu.newSubModuleClickedEvent $ Func1 $ \event -> do
        onNewModuleClicked pji $ case mi of
            mi@(ModuleInfo (Symbol prefix)) -> Just prefix
            mi -> Nothing
    menu `on` SolutionContextMenu.newDeclClickedEvent $ Func1 $ \event -> do
        doAddDeclaration pji mi $ RawDeclarationInfo $ "New Declaration"
        return False
    menu `on` SolutionContextMenu.deleteModuleClickedEvent $ Func1 $ \event -> do
        doRemoveModule pji mi
        return False
    return menu


setupUnparsableModuleContextMenu :: forall t m' p m
                                  . ( MainGuiClassIO t m' p m )
                                 => MainWindow
                                 -> ProjectInfo
                                 -> ModuleInfo
                                 -> SrcLoc
                                 -> t m ContextMenu
setupUnparsableModuleContextMenu gui pji mi loc = do
    menu <- SolutionContextMenu.makeUnparsableModuleMenu pji mi
    menu `on` SolutionContextMenu.gotoErrorClickedEvent $ Func1 $ \event -> do
        doGetItem $ UnparsableModulePath pji mi
        MainWindow.setFocus FocusEditor gui
        doGotoSrcLoc loc
        addIdleTask $ IdleThreadTask $ MainWindow.scrollEditorCursorIntoView gui
        return False
    return menu
    
setupProjectContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> t m ContextMenu
setupProjectContextMenu pji = do
    menu <- SolutionContextMenu.makeProjectMenu pji
    menu `on` SolutionContextMenu.newModuleClickedEvent $ Func1 $ \event -> do
        onNewModuleClicked pji Nothing
    menu `on` SolutionContextMenu.editProjectClickedEvent $ Func1 $ \event -> do
        onEditProjectClicked pji
        return False
    menu `on` SolutionContextMenu.deleteProjectClickedEvent $ Func1 $ \event -> do
        onDeleteProjectClicked pji
    return menu

setupSolutionContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => t m ContextMenu
setupSolutionContextMenu = do
    menu <- SolutionContextMenu.makeSolutionMenu
    menu `on` SolutionContextMenu.newProjectClickedEvent $ Func1 $ \event -> do
        onNewProjectClicked
        return False
    return menu

setupDeclContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> t m ContextMenu
setupDeclContextMenu pji mi di = do
    menu <- SolutionContextMenu.makeDeclMenu pji mi di
    menu `on` SolutionContextMenu.deleteDeclarationClickedEvent $ Func1 $ \event -> do
        doRemoveDeclaration pji mi di
        return False
    menu `on` SolutionContextMenu.exportDeclarationClickedEvent $ Func1 $ \event -> do
        onExportDeclarationClicked pji mi di
    menu `on` SolutionContextMenu.moveDeclarationClickedEvent $ Func1 $ \event -> do
        onMoveDeclarationClicked pji mi di
    menu `on` SolutionContextMenu.unExportDeclarationClickedEvent $ Func1 $ \event -> do
        doUnExportDeclaration pji mi di
        return False
    return menu

setupImportsContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
setupImportsContextMenu pji mi = id $ do
    menu <- SolutionContextMenu.makeImportsMenu pji mi
    menu `on` SolutionContextMenu.newImportClickedEvent $ Func1 $ \event -> do
        onNewImportClicked pji mi
    return menu


setupExportsContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> t m ContextMenu
setupExportsContextMenu pji mi = do
    menu <- SolutionContextMenu.makeExportsMenu pji mi
    menu `on` SolutionContextMenu.newExportClickedEvent $ Func1 $ \event -> do
        onNewExportClicked pji mi
    menu `on` SolutionContextMenu.exportAllClickedEvent $ Func1 $ \event -> do
        onExportAllClicked pji mi
    return menu

setupImportContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ImportId
                 -> t m ContextMenu
setupImportContextMenu pji mi ii = do
    menu <- SolutionContextMenu.makeImportMenu pji mi ii
    menu `on` SolutionContextMenu.deleteImportClickedEvent $ Func1 $ \event -> do
        doRemoveImport pji mi ii
        return False
    menu `on` SolutionContextMenu.editImportClickedEvent $ Func1 $ \event -> do
        onEditImportClicked pji mi ii
        return False
    return menu

setupExportContextMenu :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ExportId
                 -> t m ContextMenu
setupExportContextMenu pji mi ei = do
    menu <- SolutionContextMenu.makeExportMenu pji mi ei
    menu `on` SolutionContextMenu.deleteExportClickedEvent $ Func1 $ \event -> do
        doRemoveExport pji mi ei
        return False
    menu `on` SolutionContextMenu.editExportClickedEvent $ Func1 $ \event -> do
        onEditExportClicked pji mi ei
        return False
    return menu                        

onSolutionViewClicked :: forall t {-proxy-} m' p m
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
        pathClicked <- MainWindow.getSolutionPathClicked (BinWindowCoords x' y') gui
        menu <- case pathClicked of
            Nothing -> setupSolutionContextMenu
            Just (path, col, p) -> do
                item <- findAtPath path
                case item of
                    ProjectResult pji -> setupProjectContextMenu pji
                    ModuleResult pji mi _ -> setupModuleContextMenu pji mi
                    UnparsableModuleResult pji mi loc _ -> setupUnparsableModuleContextMenu gui pji mi loc
                    DeclResult pji mi di -> setupDeclContextMenu pji mi di
                    ImportsResult pji mi -> setupImportsContextMenu pji mi
                    ExportsResult pji mi -> setupExportsContextMenu pji mi
                    ImportResult pji mi ii -> setupImportContextMenu pji mi ii
                    ExportResult pji mi ei -> setupExportContextMenu pji mi ei
                    NoSearchResult -> setupSolutionContextMenu
        lift $ SolutionContextMenu.showMenu menu event
    return False    


onDeclEdited :: forall t {-proxy-} m' p m
               . ( MainGuiClassIO t m' p m 
                 , EditorBufferClass (t m)
                 )
              => t m ()
onDeclEdited = reapplySyntaxHighlighting

{-
onFindClicked :: forall t {-proxy-} m p 
               . ( MainGuiClass t m p  )
              => MainWindow
              -> t  IO ()
onFindClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Find
    doSetSearchMode Find

onNavigateClicked :: forall t {-proxy-} m p 
               . ( MainGuiClass t m p  )
              => MainWindow
              -> t  IO ()
onNavigateClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Navigate
    doSetSearchMode Navigate

onSearchClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m p  )
              => t  IO ()
onSearchClicked = doSearch
-}

onGotoDeclarationClicked
    :: forall t {-proxy-} m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onGotoDeclarationClicked = doGotoDeclaration

onBackClicked
    :: forall t {-proxy-} m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onBackClicked = doBackHistory

onForwardClicked
    :: forall t {-proxy-} m' p m
     . ( MainGuiClassIO t m' p m )
    => t m ()
onForwardClicked = doForwardHistory

onErrorClicked :: forall t {-proxy-} m' p m
               . ( MainGuiClass t m' p m )
              => MainWindow
              -> Gtk.TreePath
              -> TreeViewColumn
              -> t m ()
onErrorClicked gui path _ = do
    shouldFocus <- withGtkTreePath path doJumpToErrorLocation
    when shouldFocus $ MainWindow.setFocus FocusErrorList gui


onSolutionTreeTooltipQuery :: forall t m' p m
                            . ( MainGuiClass t m' p m)
                           => MainWindow
                           -> Int32
                           -> Int32
                           -> Bool
                           -> Tooltip
                           -> t m Bool
onSolutionTreeTooltipQuery gui x' y' _ tooltip = do
    let x = fromIntegral x'
        y = fromIntegral y'
    result <- MainWindow.getSolutionPathClicked (WidgetCoords x y) gui
    case result of
        Just (path, col, _) -> do
            item <- findAtPath path
            case item of
                UnparsableModuleResult _ _ loc msg -> do
                    let tooltipText = show loc ++ ": " ++ msg
                    liftIO $ tooltipSetText tooltip $ Just $ pack tooltipText
                    return True
                _ -> return False
        Nothing -> return False

{-
declBufferEdited :: GuiEnvSignal {-proxy-} m' p IO GuiComponents TextBuffer IO ()
declBufferEdited = mkGuiEnvSignal (flip withEditorBuffer id) endUserAction
-}

declBufferEdited :: SubSignalProxy GuiComponents TextBuffer TextBufferEndUserActionSignalInfo
declBufferEdited comp = withEditorBuffer comp $ \buffer -> (buffer, #endUserAction)

setupSignals :: ( MainGuiClassIO t m' p m ) => MainWindow -> t m ()
setupSignals gui = do
    gui `on` MainWindow.newClickedEvent $ Func0 $ onNewClicked
    gui `on` MainWindow.openClickedEvent $ Func0 $ onOpenClicked
    gui `on` MainWindow.digestClickedEvent $ Func0 $ onDigestClicked
    gui `on` MainWindow.saveClickedEvent $ Func0 $ onSaveClicked
    gui `on` MainWindow.saveSolutionClickedEvent $
        Func0 $ onSaveSolutionClicked
    gui `on` MainWindow.declClickedEvent $ Func2 $ onDeclClicked
    gui `on` MainWindow.projectViewClickedEvent $
        Func1 $ onSolutionViewClicked gui
    gui `on` MainWindow.buildClickedEvent $ Func0 $ onBuildClicked
    gui `on` MainWindow.runClickedEvent $ Func0 $ onRunClicked
    --gui `on_` MainWindow.findClickedEvent $ Func0 $ onFindClicked gui
    --gui `on_` MainWindow.navigateClickedEvent $ Func0 $ onNavigateClicked gui
    --gui `on_` MainWindow.searchClickedEvent $ Func0 $ onSearchClicked
    gui `on` MainWindow.gotoDeclarationClickedEvent $ 
        Func0 $ onGotoDeclarationClicked
    gui `on` MainWindow.backClickedEvent $ Func0 $ onBackClicked
    gui `on` MainWindow.forwardClickedEvent $ Func0 $ onForwardClicked
    gui `on` MainWindow.declarationEditedEvent $ Func0 $ onDeclEdited
    gui `on` MainWindow.errorClickedEvent $ Func2 $ onErrorClicked gui
    gui `on` MainWindow.windowClosedEvent $ Func0 $ liftIO exitSuccess
    gui `on` MainWindow.solutionTreeQueryTooltipEvent $ Func4 $ onSolutionTreeTooltipQuery gui
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
    
