{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
module SolutionContextMenu where

import Control.Monad.Trans

import Graphics.UI.Gtk

import Ide3.Types hiding (moduleInfo, projectInfo)

import GuiEnv

import GuiHelpers hiding (makeMenuWith)

data ContextMenu
    = ContextMenu
    { contextMenu :: Menu
    , menuItems :: ElementMenu
    }



data ElementMenu
    = ModuleMenu
    { moduleInfo :: ProjectChild ModuleInfo
    , newDeclButton :: MenuItem
    , newSubModuleButton :: MenuItem
    , renameModuleButton :: MenuItem
    , deleteModuleButton :: MenuItem
    }
    | DeclMenu
    { declInfo :: ProjectChild (ModuleChild DeclarationInfo)
    , exportDeclarationButton :: MenuItem
    , unExportDeclarationButton :: MenuItem
    , moveDeclarationButton :: MenuItem
    , deleteDeclarationButton :: MenuItem
    }
    | ImportMenu
    { importInfo :: ProjectChild (ModuleChild ImportId)
    , editImportButton :: MenuItem
    , deleteImportButton :: MenuItem
    }
    | ExportMenu
    { exportInfo :: ProjectChild (ModuleChild ExportId)
    , editExportButton :: MenuItem
    , deleteExportButton :: MenuItem
    }
    | ImportsMenu
    { importsInfo :: ProjectChild ModuleInfo
    , newImportButton :: MenuItem
    }
    | ExportsMenu
    { exportsInfo :: ProjectChild ModuleInfo
    , newExportButton :: MenuItem
    , exportAllButton :: MenuItem
    }
    | ProjectMenu
    { projectInfo :: ProjectInfo
    , newModuleButton :: MenuItem
    , deleteProjectButton :: MenuItem
    }
    | SolutionMenu
    { newProjectButton :: MenuItem
    }


makeMenuWith :: (MonadIO m) => (Menu -> m ElementMenu) -> m ContextMenu
makeMenuWith f = do
    menu <- liftIO $ menuNew
    elementMenu <- f menu
    return $ ContextMenu menu elementMenu


makeModuleMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeModuleMenu pi mi = makeMenuWith $ \menu -> do
    newDeclButton <- makeNewDeclButton menu
    newSubModuleButton <- makeNewSubModuleButton menu
    renameModuleButton <- makeRenameModuleButton menu
    deleteModuleButton <- makeDeleteModuleButton menu
    return ModuleMenu
           { moduleInfo = ProjectChild pi mi
           , newDeclButton
           , newSubModuleButton
           , renameModuleButton
           , deleteModuleButton
           }
      
makeNewDeclButton :: (MonadIO m) => Menu -> m MenuItem
makeNewDeclButton = makeMenuButton "New Declaration"

makeNewSubModuleButton :: (MonadIO m) => Menu -> m MenuItem
makeNewSubModuleButton = makeMenuButton "New Sub-Module"

makeRenameModuleButton :: (MonadIO m) => Menu -> m MenuItem
makeRenameModuleButton = makeMenuButton "Rename"

makeDeleteModuleButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteModuleButton = makeMenuButton "Delete"


makeDeclMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> DeclarationInfo -> m ContextMenu
makeDeclMenu pi mi di = makeMenuWith $ \menu -> do
    exportDeclarationButton <- makeExportDeclarationButton menu
    unExportDeclarationButton <- makeUnExportDeclarationButton menu
    moveDeclarationButton <- makeMoveDeclarationButton menu
    deleteDeclarationButton <- makeDeleteDeclarationButton menu
    return DeclMenu
           { declInfo = ProjectChild pi $ ModuleChild mi di
           , exportDeclarationButton
           , unExportDeclarationButton
           , moveDeclarationButton
           , deleteDeclarationButton
           }

makeExportDeclarationButton :: (MonadIO m) => Menu -> m MenuItem
makeExportDeclarationButton = makeMenuButton "Export"

makeUnExportDeclarationButton :: (MonadIO m) => Menu -> m MenuItem
makeUnExportDeclarationButton = makeMenuButton "Un-Export"

makeMoveDeclarationButton :: (MonadIO m) => Menu -> m MenuItem
makeMoveDeclarationButton = makeMenuButton "Move"

makeDeleteDeclarationButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteDeclarationButton = makeMenuButton "Delete"

makeImportMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> ImportId -> m ContextMenu
makeImportMenu pi mi ii = makeMenuWith $ \menu -> do
    editImportButton <- makeEditImportButton menu
    deleteImportButton <- makeDeleteImportButton menu
    return ImportMenu
           { importInfo = ProjectChild pi $ ModuleChild mi ii
           , editImportButton
           , deleteImportButton
           }

makeEditImportButton :: (MonadIO m) => Menu -> m MenuItem
makeEditImportButton = makeMenuButton "Edit"

makeDeleteImportButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteImportButton = makeMenuButton "Delete"

makeExportMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> ExportId -> m ContextMenu
makeExportMenu pi mi ei = makeMenuWith $ \menu -> do
    editExportButton <- makeEditExportButton menu
    deleteExportButton <- makeDeleteExportButton menu
    return ExportMenu
           { exportInfo = ProjectChild pi $ ModuleChild mi ei
           , editExportButton
           , deleteExportButton
           }

makeEditExportButton :: (MonadIO m) => Menu -> m MenuItem
makeEditExportButton = makeMenuButton "Edit"

makeDeleteExportButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteExportButton = makeMenuButton "Delete"

makeImportsMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeImportsMenu pi mi = makeMenuWith $ \menu -> do
    newImportButton <- makeNewImportButton menu
    return ImportsMenu
           { importsInfo = ProjectChild pi mi
           , newImportButton
           }

makeNewImportButton :: (MonadIO m) => Menu -> m MenuItem
makeNewImportButton = makeMenuButton "New Import"

makeExportsMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeExportsMenu pi mi = makeMenuWith $ \menu -> do
    newExportButton <- makeNewExportButton menu
    exportAllButton <- makeExportAllButton menu
    return ExportsMenu
           { exportsInfo = ProjectChild pi mi
           , newExportButton
           , exportAllButton
           }

makeNewExportButton :: (MonadIO m) => Menu -> m MenuItem
makeNewExportButton = makeMenuButton "New Export"

makeExportAllButton :: (MonadIO m) => Menu -> m MenuItem
makeExportAllButton = makeMenuButton "Export All"

makeProjectMenu :: (MonadIO m) => ProjectInfo -> m ContextMenu
makeProjectMenu pi = makeMenuWith $ \menu -> do
    newModuleButton <- makeNewModuleButton menu
    deleteProjectButton <- makeDeleteProjectButton menu
    return ProjectMenu
        { projectInfo = pi
        , newModuleButton
        , deleteProjectButton
        }

makeNewModuleButton :: (MonadIO m) => Menu -> m MenuItem
makeNewModuleButton = makeMenuButton "New Module"

makeDeleteProjectButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteProjectButton = makeMenuButton "Delete"

makeSolutionMenu :: (MonadIO m) => m ContextMenu
makeSolutionMenu = makeMenuWith $ \menu -> do
    newProjectButton <- makeNewProjectButton menu
    return SolutionMenu
           { newProjectButton
           }

makeNewProjectButton :: (MonadIO m) => Menu -> m MenuItem
makeNewProjectButton = makeMenuButton "New Project"


showMenu :: ContextMenu -> EventM EButton ()
showMenu (ContextMenu menu _) = do
    button <- eventButton
    time <- eventTime
    liftIO $ do
        widgetShowAll menu
        menuPopup menu $ Just (button, time)

type ContextMenuSignal proxy m' p  m object m'' a
    = GuiEnvSignal proxy m' p  m ContextMenu object m'' a


mkMenuSignal :: (Monad m, MonadIO m'') => (ElementMenu -> object)
            -> Signal object (m'' a)
            -> ContextMenuSignal proxy m' p  m object m'' a
mkMenuSignal = mkGuiEnvSignalFor menuItems

newDeclClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newDeclClickedEvent = newDeclButton `mkMenuSignal` buttonPressEvent

newSubModuleClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newSubModuleClickedEvent = newSubModuleButton `mkMenuSignal` buttonPressEvent

renameModuleClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
renameModuleClickedEvent = renameModuleButton `mkMenuSignal` buttonPressEvent

deleteModuleClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
deleteModuleClickedEvent = deleteModuleButton `mkMenuSignal` buttonPressEvent

exportDeclarationClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
exportDeclarationClickedEvent = exportDeclarationButton `mkMenuSignal` buttonPressEvent

unExportDeclarationClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
unExportDeclarationClickedEvent = unExportDeclarationButton `mkMenuSignal` buttonPressEvent

moveDeclarationClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
moveDeclarationClickedEvent = moveDeclarationButton `mkMenuSignal` buttonPressEvent

deleteDeclarationClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
deleteDeclarationClickedEvent = deleteDeclarationButton `mkMenuSignal` buttonPressEvent

editImportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
editImportClickedEvent = editImportButton `mkMenuSignal` buttonPressEvent

deleteImportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
deleteImportClickedEvent = deleteImportButton `mkMenuSignal` buttonPressEvent

editExportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
editExportClickedEvent = editExportButton `mkMenuSignal` buttonPressEvent

deleteExportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
deleteExportClickedEvent = deleteExportButton `mkMenuSignal` buttonPressEvent

newImportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newImportClickedEvent = newImportButton `mkMenuSignal` buttonPressEvent

newExportClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newExportClickedEvent = newExportButton `mkMenuSignal` buttonPressEvent

exportAllClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
exportAllClickedEvent = exportAllButton `mkMenuSignal` buttonPressEvent

newModuleClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newModuleClickedEvent = newModuleButton `mkMenuSignal` buttonPressEvent

newProjectClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
newProjectClickedEvent = newProjectButton `mkMenuSignal` buttonPressEvent

deleteProjectClickedEvent :: (Monad m) =>  ContextMenuSignal proxy m' p  m MenuItem (EventM EButton) Bool
deleteProjectClickedEvent = deleteProjectButton `mkMenuSignal` buttonPressEvent
