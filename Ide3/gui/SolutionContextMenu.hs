{-# LANGUAGE NamedFieldPuns, PolyKinds #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module SolutionContextMenu where

import Control.Monad.Trans

import GI.Gtk
import GI.Gdk

import Ide3.Types

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
    | UnparsableModuleMenu
    { moduleInfo :: ProjectChild ModuleInfo
    , gotoErrorButton :: MenuItem
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
    , editProjectButton :: MenuItem
    , deleteProjectButton :: MenuItem
    }
    | SolutionMenu
    { newProjectButton :: MenuItem
    , editSolutionButton :: MenuItem
    }


makeMenuWith :: (MonadIO m) => (Menu -> m ElementMenu) -> m ContextMenu
makeMenuWith f = do
    menu <- liftIO $ menuNew
    elementMenu <- f menu
    return $ ContextMenu menu elementMenu


makeModuleMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeModuleMenu pji mi = makeMenuWith $ \menu -> do
    newDeclButton <- makeNewDeclButton menu
    newSubModuleButton <- makeNewSubModuleButton menu
    renameModuleButton <- makeRenameModuleButton menu
    deleteModuleButton <- makeDeleteModuleButton menu
    return ModuleMenu
           { moduleInfo = ProjectChild pji mi
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

makeUnparsableModuleMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeUnparsableModuleMenu pji mi = makeMenuWith $ \menu -> do
    gotoErrorButton <- makeGotoErrorButton menu
    return UnparsableModuleMenu
            { moduleInfo = ProjectChild pji mi
            , gotoErrorButton
            }

makeGotoErrorButton :: (MonadIO m) => Menu -> m MenuItem
makeGotoErrorButton = makeMenuButton "Go to Error Location"

makeDeclMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> DeclarationInfo -> m ContextMenu
makeDeclMenu pji mi di = makeMenuWith $ \menu -> do
    exportDeclarationButton <- makeExportDeclarationButton menu
    unExportDeclarationButton <- makeUnExportDeclarationButton menu
    moveDeclarationButton <- makeMoveDeclarationButton menu
    deleteDeclarationButton <- makeDeleteDeclarationButton menu
    return DeclMenu
           { declInfo = ProjectChild pji $ ModuleChild mi di
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
makeImportMenu pji mi ii = makeMenuWith $ \menu -> do
    editImportButton <- makeEditImportButton menu
    deleteImportButton <- makeDeleteImportButton menu
    return ImportMenu
           { importInfo = ProjectChild pji $ ModuleChild mi ii
           , editImportButton
           , deleteImportButton
           }

makeEditImportButton :: (MonadIO m) => Menu -> m MenuItem
makeEditImportButton = makeMenuButton "Edit"

makeDeleteImportButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteImportButton = makeMenuButton "Delete"

makeExportMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> ExportId -> m ContextMenu
makeExportMenu pji mi ei = makeMenuWith $ \menu -> do
    editExportButton <- makeEditExportButton menu
    deleteExportButton <- makeDeleteExportButton menu
    return ExportMenu
           { exportInfo = ProjectChild pji $ ModuleChild mi ei
           , editExportButton
           , deleteExportButton
           }

makeEditExportButton :: (MonadIO m) => Menu -> m MenuItem
makeEditExportButton = makeMenuButton "Edit"

makeDeleteExportButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteExportButton = makeMenuButton "Delete"

makeImportsMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeImportsMenu pji mi = makeMenuWith $ \menu -> do
    newImportButton <- makeNewImportButton menu
    return ImportsMenu
           { importsInfo = ProjectChild pji mi
           , newImportButton
           }

makeNewImportButton :: (MonadIO m) => Menu -> m MenuItem
makeNewImportButton = makeMenuButton "New Import"

makeExportsMenu :: (MonadIO m) => ProjectInfo -> ModuleInfo -> m ContextMenu
makeExportsMenu pji mi = makeMenuWith $ \menu -> do
    newExportButton <- makeNewExportButton menu
    exportAllButton <- makeExportAllButton menu
    return ExportsMenu
           { exportsInfo = ProjectChild pji mi
           , newExportButton
           , exportAllButton
           }

makeNewExportButton :: (MonadIO m) => Menu -> m MenuItem
makeNewExportButton = makeMenuButton "New Export"

makeExportAllButton :: (MonadIO m) => Menu -> m MenuItem
makeExportAllButton = makeMenuButton "Export All"

makeProjectMenu :: (MonadIO m) => ProjectInfo -> m ContextMenu
makeProjectMenu pji = makeMenuWith $ \menu -> do
    newModuleButton <- makeNewModuleButton menu
    editProjectButton <- makeEditProjectButton menu
    deleteProjectButton <- makeDeleteProjectButton menu
    return ProjectMenu
        { projectInfo = pji
        , newModuleButton
        , editProjectButton
        , deleteProjectButton
        }

makeNewModuleButton :: (MonadIO m) => Menu -> m MenuItem
makeNewModuleButton = makeMenuButton "New Module"

makeEditProjectButton :: (MonadIO m) => Menu -> m MenuItem
makeEditProjectButton = makeMenuButton "Edit"

makeDeleteProjectButton :: (MonadIO m) => Menu -> m MenuItem
makeDeleteProjectButton = makeMenuButton "Delete"

makeSolutionMenu :: (MonadIO m) => m ContextMenu
makeSolutionMenu = makeMenuWith $ \menu -> do
    newProjectButton <- makeNewProjectButton menu
    editSolutionButton <- makeEditSolutionButton menu
    return SolutionMenu
           { newProjectButton
           , editSolutionButton
           }

makeNewProjectButton :: (MonadIO m) => Menu -> m MenuItem
makeNewProjectButton = makeMenuButton "New Project"

makeEditSolutionButton :: (MonadIO m) => Menu -> m MenuItem
makeEditSolutionButton = makeMenuButton "Edit"


showMenu :: MonadIO m => ContextMenu -> EventButton -> m ()
showMenu (ContextMenu menu _) _ = do
    --button <- get event #button
    --time <- get event #time
    widgetShowAll menu
    --menuPopup menu noMenuShell noMenuItem noMenuPositionFunc button time
    menuPopupAtPointer menu Nothing

{-
type ContextMenuSignal proxy m' p  m object m'' a
    = GuiEnvSignal proxy m' p  m ContextMenu object m'' a
-}

type ContextMenuSignal subObject info = SubSignalProxy ContextMenu subObject info

mkMenuSignal :: (ElementMenu -> object)
             -> SignalProxy object info 
             -> ContextMenuSignal object info
mkMenuSignal object signal = \contextMenu -> (object $ menuItems contextMenu, signal)


newDeclClickedEvent ::   ContextMenuSignal MenuItem WidgetButtonPressEventSignalInfo
newDeclClickedEvent = newDeclButton `mkMenuSignal` #buttonPressEvent

newSubModuleClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
newSubModuleClickedEvent = newSubModuleButton `mkMenuSignal` #buttonPressEvent

renameModuleClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
renameModuleClickedEvent = renameModuleButton `mkMenuSignal` #buttonPressEvent

deleteModuleClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
deleteModuleClickedEvent = deleteModuleButton `mkMenuSignal` #buttonPressEvent

gotoErrorClickedEvent :: ContextMenuSignal MenuItem WidgetButtonPressEventSignalInfo
gotoErrorClickedEvent = gotoErrorButton `mkMenuSignal` #buttonPressEvent

exportDeclarationClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
exportDeclarationClickedEvent = exportDeclarationButton `mkMenuSignal` #buttonPressEvent

unExportDeclarationClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
unExportDeclarationClickedEvent = unExportDeclarationButton `mkMenuSignal` #buttonPressEvent

moveDeclarationClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
moveDeclarationClickedEvent = moveDeclarationButton `mkMenuSignal` #buttonPressEvent

deleteDeclarationClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
deleteDeclarationClickedEvent = deleteDeclarationButton `mkMenuSignal` #buttonPressEvent

editImportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
editImportClickedEvent = editImportButton `mkMenuSignal` #buttonPressEvent

deleteImportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
deleteImportClickedEvent = deleteImportButton `mkMenuSignal` #buttonPressEvent

editExportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
editExportClickedEvent = editExportButton `mkMenuSignal` #buttonPressEvent

deleteExportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
deleteExportClickedEvent = deleteExportButton `mkMenuSignal` #buttonPressEvent

newImportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
newImportClickedEvent = newImportButton `mkMenuSignal` #buttonPressEvent

newExportClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
newExportClickedEvent = newExportButton `mkMenuSignal` #buttonPressEvent

exportAllClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
exportAllClickedEvent = exportAllButton `mkMenuSignal` #buttonPressEvent

newModuleClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
newModuleClickedEvent = newModuleButton `mkMenuSignal` #buttonPressEvent

newProjectClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
newProjectClickedEvent = newProjectButton `mkMenuSignal` #buttonPressEvent

editSolutionClickedEvent :: ContextMenuSignal MenuItem WidgetButtonPressEventSignalInfo
editSolutionClickedEvent = editSolutionButton `mkMenuSignal` #buttonPressEvent

editProjectClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
editProjectClickedEvent = editProjectButton `mkMenuSignal` #buttonPressEvent

deleteProjectClickedEvent ::   ContextMenuSignal  MenuItem WidgetButtonPressEventSignalInfo
deleteProjectClickedEvent = deleteProjectButton `mkMenuSignal` #buttonPressEvent
