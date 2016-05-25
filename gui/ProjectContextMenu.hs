module ProjectContextMenu where

import Control.Monad.Trans

import Graphics.UI.Gtk

import Ide3.Types

import GuiHelpers hiding (makeMenuWith)

data ContextMenu
    = ContextMenu
    { contextMenu :: Menu
    , menuItems :: ElementMenu
    }



data ElementMenu
    = ModuleMenu
    { moduleInfo :: ModuleInfo
    , newDeclButton :: MenuItem
    , newSubModuleButton :: MenuItem
    , renameModuleButton :: MenuItem
    , deleteModuleButton :: MenuItem
    }
    | DeclMenu
    { declInfo :: ModuleChild DeclarationInfo
    , moveDeclarationButton :: MenuItem
    , deleteDeclarationButton :: MenuItem
    }
    | ImportMenu
    { importInfo :: ModuleChild ImportId
    , editImportButton :: MenuItem
    , deleteImportButton :: MenuItem
    }
    | ExportMenu
    { exportInfo :: ModuleChild ExportId
    , editExportButton :: MenuItem
    , deleteExportButton :: MenuItem
    }
    | ImportsMenu
    { importsInfo :: ModuleInfo
    , newImportButton :: MenuItem
    }
    | ExportsMenu
    { exportsInfo :: ModuleInfo
    , newExportButton :: MenuItem
    , exportAllButton :: MenuItem
    }
    | ProjectMenu
    { newModuleButton :: MenuItem
    }


makeMenuWith f = do
    menu <- menuNew
    elementMenu <- f menu
    return $ ContextMenu menu elementMenu


makeModuleMenu :: ModuleInfo -> IO ContextMenu
makeModuleMenu mi = makeMenuWith $ \menu -> do
    newDeclButton <- makeNewDeclButton menu
    newSubModuleButton <- makeNewSubModuleButton menu
    renameModuleButton <- makeRenameModuleButton menu
    deleteModuleButton <- makeDeleteModuleButton menu
    return $
        ModuleMenu
            mi
            newDeclButton
            newSubModuleButton
            renameModuleButton
            deleteModuleButton
      
makeNewDeclButton = makeMenuButton "New Declaration"
makeNewSubModuleButton = makeMenuButton "New Sub-Module"
makeRenameModuleButton = makeMenuButton "Rename"
makeDeleteModuleButton = makeMenuButton "Delete"


makeDeclMenu :: ModuleInfo -> DeclarationInfo -> IO ContextMenu
makeDeclMenu mi di = makeMenuWith $ \menu -> do
    moveDeclarationButton <- makeMoveDeclarationButton menu
    deleteDeclarationButton <- makeDeleteDeclarationButton menu
    return $
        DeclMenu
            (ModuleChild mi di)
            moveDeclarationButton
            deleteDeclarationButton

makeMoveDeclarationButton = makeMenuButton "Move"
makeDeleteDeclarationButton = makeMenuButton "Delete"

makeImportMenu :: ModuleInfo -> ImportId -> IO ContextMenu
makeImportMenu mi ii = makeMenuWith $ \menu -> do
    editImportButton <- makeEditImportButton menu
    deleteImportButton <- makeDeleteImportButton menu
    return $ 
        ImportMenu
            (ModuleChild mi ii)
            editImportButton
            deleteImportButton

makeEditImportButton = makeMenuButton "Edit"
makeDeleteImportButton = makeMenuButton "Delete"

makeExportMenu :: ModuleInfo -> ExportId -> IO ContextMenu
makeExportMenu mi ei = makeMenuWith $ \menu -> do
    editExportButton <- makeEditExportButton menu
    deleteExportButton <- makeDeleteExportButton menu
    return $ 
        ExportMenu
            (ModuleChild mi ei)
            editExportButton
            deleteExportButton

makeEditExportButton = makeMenuButton "Edit"
makeDeleteExportButton = makeMenuButton "Delete"

makeImportsMenu :: ModuleInfo -> IO ContextMenu
makeImportsMenu mi = makeMenuWith $ \menu -> do
    newImportButton <- makeNewImportButton menu
    return $
        ImportsMenu
            mi
            newImportButton

makeNewImportButton = makeMenuButton "New Import"

makeExportsMenu :: ModuleInfo -> IO ContextMenu
makeExportsMenu mi = makeMenuWith $ \menu -> do
    newExportButton <- makeNewExportButton menu
    exportAllButton <- makeExportAllButton menu
    return $
        ExportsMenu
            mi
            newExportButton
            exportAllButton

makeNewExportButton = makeMenuButton "New Export"
makeExportAllButton = makeMenuButton "Export All"

makeProjectMenu :: IO ContextMenu
makeProjectMenu = makeMenuWith $ \menu -> do
    newModuleButton <- makeNewModuleButton menu
    return $
        ProjectMenu
            newModuleButton

makeNewModuleButton = makeMenuButton "New Module"

showMenu (ContextMenu menu _) = do
    button <- eventButton
    time <- eventTime
    liftIO $ do
        widgetShowAll menu
        menuPopup menu $ Just (button, time)



mkMenuSignal obj event = (obj . menuItems) `mkGuiSignal` event

newDeclClickedEvent = newDeclButton `mkMenuSignal` buttonPressEvent
newSubModuleClickedEvent = newSubModuleButton `mkMenuSignal` buttonPressEvent
renameModuleClickedEvent = renameModuleButton `mkMenuSignal` buttonPressEvent
deleteModuleClickedEvent = deleteModuleButton `mkMenuSignal` buttonPressEvent
moveDeclarationClickedEvent = moveDeclarationButton `mkMenuSignal` buttonPressEvent
deleteDeclarationClickedEvent = deleteDeclarationButton `mkMenuSignal` buttonPressEvent
editImportClickedEvent = editImportButton `mkMenuSignal` buttonPressEvent
deleteImportClickedEvent = deleteImportButton `mkMenuSignal` buttonPressEvent
editExportClickedEvent = editExportButton `mkMenuSignal` buttonPressEvent
deleteExportClickedEvent = deleteExportButton `mkMenuSignal` buttonPressEvent
newImportClickedEvent = newImportButton `mkMenuSignal` buttonPressEvent
newExportClickedEvent = newExportButton `mkMenuSignal` buttonPressEvent
exportAllClickedEvent = exportAllButton `mkMenuSignal` buttonPressEvent
newModuleClickedEvent = newModuleButton `mkMenuSignal` buttonPressEvent
