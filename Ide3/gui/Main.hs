{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

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

import Graphics.UI.Gtk hiding (get, TreePath)

import Ide3.Types
import Ide3.Types.State
import Ide3.NewMonad
import Ide3.NewMonad.Instances.State.Class
import Ide3.NewMonad.Instances.State.Class.Instances.Strict
import Ide3.Digest
import qualified Ide3.Solution as Solution

import Viewer
import ViewerMonad
import ViewerMonad2

import SolutionTree

import GuiMonad
import GuiCommand
import GuiEnv

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

import PseudoState

import GuiViewer
import GuiViewer.Class

import CabalFilesystemSolution

import Initializer

import GuiClass
import GuiClass.GuiEnv

deriving instance (MonadMask m) => MonadMask (StatefulWrapper m)
deriving instance (MonadCatch m) => MonadCatch (StatefulWrapper m)
deriving instance (MonadThrow m) => MonadThrow (StatefulWrapper m)

deriving instance (MonadMask m) => MonadMask (SolutionStateT m)
deriving instance (MonadCatch m) => MonadCatch (SolutionStateT m)
deriving instance (MonadThrow m) => MonadThrow (SolutionStateT m)

instance (InteruptMonad2 s m) => InteruptMonad2 s (StatefulWrapper m) where
    interupt2 x f = interupt2 x (runStatefulWrapper f)

instance PseudoStateT SolutionStateT Solution where
    runPseudoStateT = runStateT . runSolutionStateT

deriving instance MonadMask GtkIO
deriving instance MonadCatch GtkIO
deriving instance MonadThrow GtkIO
deriving instance InteruptMonad0 GtkIO

instance ErrorClass (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))) where
    displayError msg = lift $ lift $ lift $ lift $ lift $ displayError msg

declBufferEdited :: GuiEnvSignal proxy m' p IO GuiComponents TextBuffer IO ()
declBufferEdited = mkGuiEnvSignal (flip withEditorBuffer id) endUserAction

type MainGuiClass m p = 
    ( MonadIO m
    , ViewerMonad m
    , PersistentSolutionMonad m
    , MonadMask m
    , ErrorClass m
    , GuiViewerClass m
    , ViewerStateClass m
    , InteruptMonad1 (MVar (MVarType p)) m
    )

type MainGuiClassIO m p m' = ( MainGuiClass m p , MonadIO m' )

onNewSolutionConfirmed :: forall proxy m p 
                       . ( MainGuiClass m p  )
                      => NewSolutionDialog
                      -> GuiEnvT proxy m p IO ()
onNewSolutionConfirmed dialog = do
    projectRoot <- liftIO $ NewSolutionDialog.getSelectedFolder dialog
    projectName <- liftIO $ NewSolutionDialog.getSolutionName dialog
    templateName <- liftIO $ NewSolutionDialog.getTemplateName dialog
    doNew projectRoot projectName templateName
    liftIO $ NewSolutionDialog.close dialog
    

onNewClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onNewClicked = do
    env <- getEnv
    NewSolutionDialog.make $ \gui -> do
        liftIO $ gui `onGui` NewSolutionDialog.confirmClicked $ do
            liftIO $ runGuiEnvT (onNewSolutionConfirmed gui) env
            return False
        liftIO $ gui `onGui` NewSolutionDialog.cancelClicked $ do
            liftIO $ NewSolutionDialog.close gui
            return False
    return ()

onOpenClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onOpenClicked = do
    dialog <- liftIO $ fileChooserDialogNew
        Nothing
        Nothing
        FileChooserActionOpen
        [ ("Close", ResponseReject)
        , ("Open" , ResponseAccept)
        ]
    r <- liftIO $ dialogRun dialog
    case r of
        ResponseAccept -> do
            Just path <- liftIO $ fileChooserGetFilename dialog
            liftIO $ widgetDestroy dialog
            mapGuiEnv liftIO $ doOpen path
            liftIO $ setCurrentDirectory $ takeDirectory path
        ResponseReject -> liftIO $ widgetDestroy dialog
        _ -> liftIO $ widgetDestroy dialog

onDigestClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onDigestClicked = do
    dialog <- liftIO $ fileChooserDialogNew
        Nothing
        Nothing
        FileChooserActionSelectFolder
        [ ("Close", ResponseReject)
        , ("Open" , ResponseAccept)
        ]
    r <- liftIO $ dialogRun dialog
    case r of
        ResponseAccept -> do
            Just path <- liftIO $ fileChooserGetFilename dialog
            liftIO $ widgetDestroy dialog
            mapGuiEnv liftIO $ doOpen path
            liftIO $ setCurrentDirectory path
        ResponseReject -> liftIO $ widgetDestroy dialog



onDeclClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => TreePath
              -> TreeViewColumn
              -> GuiEnvT proxy m p  IO ()
onDeclClicked path _ = doGetDecl path

onBuildClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onBuildClicked = void $ mapGuiEnv liftIO $ doBuild

onRunClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onRunClicked = mapGuiEnv liftIO $ doRun


onSaveClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onSaveClicked = mapGuiEnv liftIO $ doSave

onSaveSolutionClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onSaveSolutionClicked = mapGuiEnv liftIO $ doSaveSolution Nothing

onNewProjectClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  (EventM EButton) Bool
onNewProjectClicked = undefined

onDeleteProjectClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onDeleteProjectClicked pi = undefined
    


onNewModuleClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> Maybe String
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onNewModuleClicked pi modName = do
    NewModuleDialog.make modName $ \dialog -> do
        dialog `onGuiM` NewModuleDialog.confirmClickedEvent $ do
            moduleName <- NewModuleDialog.getModuleName dialog
            case moduleName of
                "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter a module name" ""
                name -> do
                    mapGuiEnv liftIO $ doAddModule pi (ModuleInfo (Symbol moduleName))
                    NewModuleDialog.close dialog
            return False
        dialog `onGuiM` NewModuleDialog.cancelClickedEvent $ do
            NewModuleDialog.close dialog
            return False
    return False

onNewImportClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onNewImportClicked pi mi = do
    NewImportDialog.makeNew $ \dialog -> do
        dialog `onGuiM` NewImportDialog.confirmClickedEvent $ do
            import_ <- NewImportDialog.getImport dialog
            case import_ of
                "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter an import" ""
                import_ -> do
                    maybeError <- mapGuiEnv liftIO $ doAddImport pi mi import_
                    case maybeError of
                        Just err -> mapGuiEnv liftIO $ doError err
                        Nothing -> NewImportDialog.close dialog
            return False
        dialog `onGuiM` NewImportDialog.cancelClickedEvent $ do
            NewImportDialog.close dialog
            return False
    return False


onEditImportClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> ImportId
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onEditImportClicked pi mi ii = do
    getResult <- mapGuiEnv liftIO $ doGetImport pi mi ii
    case getResult of
        Nothing -> return False
        Just importStr -> do
            NewImportDialog.makeEdit importStr $ \dialog -> do
                dialog `onGuiM` NewImportDialog.confirmClickedEvent $ do
                    import_ <- NewImportDialog.getImport dialog
                    case import_ of
                        "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter an import" ""
                        import_ -> do
                            maybeError <- mapGuiEnv liftIO $ doEditImport pi mi ii import_
                            case maybeError of
                                Just err -> mapGuiEnv liftIO $ doError err
                                Nothing -> NewImportDialog.close dialog
                    return False
                dialog `onGuiM` NewImportDialog.cancelClickedEvent $ do
                    NewImportDialog.close dialog
                    return False
            return False

onNewExportClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onNewExportClicked pi mi = do
    NewExportDialog.makeNew $ \dialog -> do
        dialog `onGuiM` NewExportDialog.confirmClickedEvent $ do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- mapGuiEnv liftIO $ doAddExport pi mi export_
                    case maybeError of
                        Just err -> mapGuiEnv liftIO $ doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `onGuiM` NewExportDialog.cancelClickedEvent $ do
            NewExportDialog.close dialog
            return False
    return False

onExportAllClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onExportAllClicked pi mi = mapGuiEnv liftIO $ do
    doExportAll pi mi
    return False
    

onEditExportClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> ExportId
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onEditExportClicked pi mi ii = do
    getResult <- mapGuiEnv liftIO $ doGetExport pi mi ii
    case getResult of
        Nothing -> return False
        Just exportStr -> do
            NewExportDialog.makeEdit exportStr $ \dialog -> do
                dialog `onGuiM` NewExportDialog.confirmClickedEvent $ do
                    export_ <- NewExportDialog.getExport dialog
                    case export_ of
                        "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter an export" ""
                        export_ -> do
                            maybeError <- mapGuiEnv liftIO $ doEditExport pi mi ii export_
                            case maybeError of
                                Just err -> mapGuiEnv liftIO $ doError err
                                Nothing -> NewExportDialog.close dialog
                    return False
                dialog `onGuiM` NewExportDialog.cancelClickedEvent $ do
                    NewExportDialog.close dialog
                    return False
            return False

onExportDeclarationClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => ProjectInfo
              -> ModuleInfo
              -> DeclarationInfo
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onExportDeclarationClicked pi mi (DeclarationInfo (Symbol declStr)) = do
    NewExportDialog.makeEdit declStr $ \dialog -> do
        dialog `onGuiM` NewExportDialog.confirmClickedEvent $ do
            export_ <- NewExportDialog.getExport dialog
            case export_ of
                "" -> mapGuiEnv liftIO $ doError $ InvalidOperation "Please enter an export" ""
                export_ -> do
                    maybeError <- mapGuiEnv liftIO $ doAddExport pi mi export_
                    case maybeError of
                        Just err -> mapGuiEnv liftIO $ doError err
                        Nothing -> NewExportDialog.close dialog
            return False
        dialog `onGuiM` NewExportDialog.cancelClickedEvent $ do
            NewExportDialog.close dialog
            return False
    return False


setupModuleContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT proxy m p  m' ContextMenu
setupModuleContextMenu pi mi = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeModuleMenu pi mi
    menu `onGuiM` SolutionContextMenu.newSubModuleClickedEvent $ do
        onNewModuleClicked pi $ case mi of
            mi@(ModuleInfo (Symbol prefix)) -> Just prefix
            mi -> Nothing
    menu `onGuiM` SolutionContextMenu.newDeclClickedEvent $ do
        mapGuiEnv liftIO $ doAddDeclaration pi mi $ DeclarationInfo $ Symbol "New Declaration"
        return False
    menu `onGuiM` SolutionContextMenu.deleteModuleClickedEvent $ do
        mapGuiEnv liftIO $ doRemoveModule pi mi
        return False
    return menu


setupProjectContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> GuiEnvT proxy m p  m' ContextMenu
setupProjectContextMenu pi = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeProjectMenu pi
    menu `onGuiM` SolutionContextMenu.newModuleClickedEvent $ do
        onNewModuleClicked pi Nothing
    menu `onGuiM` SolutionContextMenu.deleteProjectClickedEvent $ do
        onDeleteProjectClicked pi
    return menu

setupSolutionContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => GuiEnvT proxy m p  m' ContextMenu
setupSolutionContextMenu = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeSolutionMenu
    menu `onGuiM` SolutionContextMenu.newProjectClickedEvent $ do
        onNewProjectClicked
    return menu

setupDeclContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> DeclarationInfo
                 -> GuiEnvT proxy m p  m' ContextMenu
setupDeclContextMenu pi mi di = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeDeclMenu pi mi di
    menu `onGuiM` SolutionContextMenu.deleteDeclarationClickedEvent $ mapGuiEnv liftIO $ do
        doRemoveDeclaration pi mi di
        return False
    menu `onGuiM` SolutionContextMenu.exportDeclarationClickedEvent $ do
        onExportDeclarationClicked pi mi di
    menu `onGuiM` SolutionContextMenu.unExportDeclarationClickedEvent $ mapGuiEnv liftIO $ do
        doUnExportDeclaration pi mi di
        return False
    return menu

setupImportsContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT proxy m p  m' ContextMenu
setupImportsContextMenu pi mi = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeImportsMenu pi mi
    menu `onGuiM` SolutionContextMenu.newImportClickedEvent $ onNewImportClicked pi mi
    return menu


setupExportsContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> GuiEnvT proxy m p  m' ContextMenu
setupExportsContextMenu pi mi = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeExportsMenu pi mi
    menu `onGuiM` SolutionContextMenu.newExportClickedEvent $ onNewExportClicked pi mi
    menu `onGuiM` SolutionContextMenu.exportAllClickedEvent $ onExportAllClicked pi mi
    return menu

setupImportContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ImportId
                 -> GuiEnvT proxy m p  m' ContextMenu
setupImportContextMenu pi mi ii = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeImportMenu pi mi ii
    menu `onGuiM` SolutionContextMenu.deleteImportClickedEvent $ mapGuiEnv liftIO $ do
        doRemoveImport pi mi ii
        return False
    menu `onGuiM` SolutionContextMenu.editImportClickedEvent $ do
        onEditImportClicked pi mi ii
        return False
    return menu

setupExportContextMenu :: forall proxy m p  m'
               . ( MainGuiClassIO m p  m' )
                 => ProjectInfo
                 -> ModuleInfo
                 -> ExportId
                 -> GuiEnvT proxy m p  m' ContextMenu
setupExportContextMenu pi mi ei = mapGuiEnv liftIO $ do
    menu <- SolutionContextMenu.makeExportMenu pi mi ei
    menu `onGuiM` SolutionContextMenu.deleteExportClickedEvent $ mapGuiEnv liftIO $ do
        doRemoveExport pi mi ei
        return False
    menu `onGuiM` SolutionContextMenu.editExportClickedEvent $ do
        onEditExportClicked pi mi ei
        return False
    return menu                        

onSolutionViewClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => MainWindow
              -> GuiEnvT proxy m p  (EventM EButton) Bool
onSolutionViewClicked gui = do
    button <- lift $ eventButton
    when (button == RightButton) $ do
        (x,y) <- lift $ eventCoordinates
        time <- lift $ eventTime
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
        lift $ SolutionContextMenu.showMenu menu
    return False    


onDeclEdited :: forall proxy m p m'
               . ( MainGuiClassIO m p m' )
              => GuiEnvT proxy m p m' ()
onDeclEdited = reapplySyntaxHighlighting

{-
onFindClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => MainWindow
              -> GuiEnvT proxy m p  IO ()
onFindClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Find
    doSetSearchMode Find

onNavigateClicked :: forall proxy m p 
               . ( MainGuiClass m p  )
              => MainWindow
              -> GuiEnvT proxy m p  IO ()
onNavigateClicked window = do
    MainWindow.setSearchBarVisible window True
    MainWindow.setSearchMode window Navigate
    doSetSearchMode Navigate

onSearchClicked :: forall proxy m p  m'
               . ( MainGuiClass m p  )
              => GuiEnvT proxy m p  IO ()
onSearchClicked = doSearch
-}

onGotoDeclarationClicked
    :: forall proxy m p m'
     . ( MainGuiClassIO m p m' )
    => GuiEnvT proxy m p m' ()
onGotoDeclarationClicked = doGotoDeclaration

onBackClicked
    :: forall proxy m p m'
     . ( MainGuiClassIO m p m' )
    => GuiEnvT proxy m p m' ()
onBackClicked = doBackHistory

onForwardClicked
    :: forall proxy m p m'
     . ( MainGuiClassIO m p m' )
    => GuiEnvT proxy m p m' ()
onForwardClicked = doForwardHistory

setupSignals gui = do
    gui `onGuiM` MainWindow.newClickedEvent $ onNewClicked
    gui `onGuiM` MainWindow.openClickedEvent $ onOpenClicked
    gui `onGuiM` MainWindow.digestClickedEvent $ onDigestClicked
    gui `onGuiM` MainWindow.saveClickedEvent $ onSaveClicked
    gui `onGuiM` MainWindow.saveSolutionClickedEvent $ onSaveSolutionClicked
    gui `onGuiF` MainWindow.declClickedEvent $ Compose onDeclClicked
    gui `onGuiM` MainWindow.projectViewClickedEvent $ onSolutionViewClicked gui
    gui `onGuiM` MainWindow.buildClickedEvent $ onBuildClicked
    gui `onGuiM` MainWindow.runClickedEvent $ onRunClicked
    --gui `onGuiM` MainWindow.findClickedEvent $ onFindClicked gui
    --gui `onGuiM` MainWindow.navigateClickedEvent $ onNavigateClicked gui
    --gui `onGuiM` MainWindow.searchClickedEvent $ onSearchClicked
    gui `onGuiM` MainWindow.gotoDeclarationClickedEvent $ onGotoDeclarationClicked
    gui `onGuiM` MainWindow.backClickedEvent $ onBackClicked
    gui `onGuiM` MainWindow.forwardClickedEvent $ onForwardClicked
    gui `onGuiM` MainWindow.windowClosedEvent $ do
        liftIO exitSuccess
        return False
    withGuiComponents $ \comp  -> do
        comp `afterGuiM` declBufferEdited $ do
            onDeclEdited

setupKeyboardShortcuts gui group = liftIO $ do
    gui `MainWindow.addAccelGroup` group
    MainWindow.addNewClickedEventAccelerator gui group
        "n" [Control, Shift] [AccelVisible]
    MainWindow.addOpenClickedEventAccelerator gui group
        "o" [Control] [AccelVisible]
    MainWindow.addDigestClickedEventAccelerator gui group
        "o" [Control, Shift] [AccelVisible]
    MainWindow.addSaveClickedEventAccelerator gui group
        "s" [Control] [AccelVisible]
    MainWindow.addSaveSolutionClickedEventAccelerator gui group
        "s" [Control,Shift] [AccelVisible]
    MainWindow.addBuildClickedEventAccelerator gui group
        "F5" [] [AccelVisible]
    {-MainWindow.addFindClickedEventAccelerator gui group
        "f" [Control] [AccelVisible]-}
    {-MainWindow.addNavigateClickedEventAccelerator gui group
        "KP_Space" [Control] [AccelVisible]-}
    MainWindow.addGotoDeclarationEventAccelerator gui group
        "d" [Control] [AccelVisible]
    MainWindow.addBackEventAccelerator gui group
        "less" [Control] [AccelVisible]
    MainWindow.addForwardEventAccelerator gui group
        "greater" [Control] [AccelVisible]
    
doMain :: forall proxy m p 
        . ( MainGuiClass m p )
       => proxy m 
       -> p
       -> IO ()
doMain proxy init = do
    projectMVar <- newMVar (emptyGuiViewer,(emptyViewer, init))
    components <- initializeComponents
    manager <- uiManagerNew
    group <- uiManagerGetAccelGroup manager
    let env = GuiEnv proxy components projectMVar
    flip runGuiEnvT env $ do
        withGuiComponents $ liftIO . applyDeclBufferAttrs defaultTextAttrs
        MainWindow.make $ \gui -> do
            setupSignals gui
            setupKeyboardShortcuts gui group
    return ()


main :: IO ()
main = doMain (Proxy :: Proxy (GuiViewerT (ViewerStateT (CabalSolution (StatefulWrapper (SolutionStateT GtkIO))))))
              (Unopened, Solution.empty)
