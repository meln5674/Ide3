{-# LANGUAGE NamedFieldPuns #-}
module ProjectContextMenu where

import Control.Monad.Trans

import Graphics.UI.Gtk

import Ide3.Types hiding (moduleInfo)

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


makeMenuWith :: (Menu -> IO ElementMenu) -> IO ContextMenu
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
    return ModuleMenu
           { moduleInfo = mi
           , newDeclButton
           , newSubModuleButton
           , renameModuleButton
           , deleteModuleButton
           }
      
makeNewDeclButton :: Menu -> IO MenuItem
makeNewDeclButton = makeMenuButton "New Declaration"

makeNewSubModuleButton :: Menu -> IO MenuItem
makeNewSubModuleButton = makeMenuButton "New Sub-Module"

makeRenameModuleButton :: Menu -> IO MenuItem
makeRenameModuleButton = makeMenuButton "Rename"

makeDeleteModuleButton :: Menu -> IO MenuItem
makeDeleteModuleButton = makeMenuButton "Delete"


makeDeclMenu :: ModuleInfo -> DeclarationInfo -> IO ContextMenu
makeDeclMenu mi di = makeMenuWith $ \menu -> do
    moveDeclarationButton <- makeMoveDeclarationButton menu
    deleteDeclarationButton <- makeDeleteDeclarationButton menu
    return DeclMenu
           { declInfo = ModuleChild mi di
           , moveDeclarationButton
           , deleteDeclarationButton
           }

makeMoveDeclarationButton :: Menu -> IO MenuItem
makeMoveDeclarationButton = makeMenuButton "Move"

makeDeleteDeclarationButton :: Menu -> IO MenuItem
makeDeleteDeclarationButton = makeMenuButton "Delete"

makeImportMenu :: ModuleInfo -> ImportId -> IO ContextMenu
makeImportMenu mi ii = makeMenuWith $ \menu -> do
    editImportButton <- makeEditImportButton menu
    deleteImportButton <- makeDeleteImportButton menu
    return ImportMenu
           { importInfo = ModuleChild mi ii
           , editImportButton
           , deleteImportButton
           }

makeEditImportButton :: Menu -> IO MenuItem
makeEditImportButton = makeMenuButton "Edit"

makeDeleteImportButton :: Menu -> IO MenuItem
makeDeleteImportButton = makeMenuButton "Delete"

makeExportMenu :: ModuleInfo -> ExportId -> IO ContextMenu
makeExportMenu mi ei = makeMenuWith $ \menu -> do
    editExportButton <- makeEditExportButton menu
    deleteExportButton <- makeDeleteExportButton menu
    return ExportMenu
           { exportInfo = ModuleChild mi ei
           , editExportButton
           , deleteExportButton
           }

makeEditExportButton :: Menu -> IO MenuItem
makeEditExportButton = makeMenuButton "Edit"

makeDeleteExportButton :: Menu -> IO MenuItem
makeDeleteExportButton = makeMenuButton "Delete"

makeImportsMenu :: ModuleInfo -> IO ContextMenu
makeImportsMenu mi = makeMenuWith $ \menu -> do
    newImportButton <- makeNewImportButton menu
    return ImportsMenu
           { importsInfo = mi
           , newImportButton
           }

makeNewImportButton :: Menu -> IO MenuItem
makeNewImportButton = makeMenuButton "New Import"

makeExportsMenu :: ModuleInfo -> IO ContextMenu
makeExportsMenu mi = makeMenuWith $ \menu -> do
    newExportButton <- makeNewExportButton menu
    exportAllButton <- makeExportAllButton menu
    return ExportsMenu
           { exportsInfo = mi
           , newExportButton
           , exportAllButton
           }

makeNewExportButton :: Menu -> IO MenuItem
makeNewExportButton = makeMenuButton "New Export"

makeExportAllButton :: Menu -> IO MenuItem
makeExportAllButton = makeMenuButton "Export All"

makeProjectMenu :: IO ContextMenu
makeProjectMenu = makeMenuWith $ \menu -> do
    newModuleButton <- makeNewModuleButton menu
    return ProjectMenu
           { newModuleButton
           }

makeNewModuleButton :: Menu -> IO MenuItem
makeNewModuleButton = makeMenuButton "New Module"

showMenu :: ContextMenu -> EventM EButton ()
showMenu (ContextMenu menu _) = do
    button <- eventButton
    time <- eventTime
    liftIO $ do
        widgetShowAll menu
        menuPopup menu $ Just (button, time)

type ContextMenuSignal = GuiSignal ContextMenu

mkMenuSignal :: (ElementMenu -> object)
            -> Signal object handler 
            -> ContextMenuSignal object handler
mkMenuSignal obj event = (obj . menuItems) `mkGuiSignal` event

newDeclClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
newDeclClickedEvent = newDeclButton `mkMenuSignal` buttonPressEvent

newSubModuleClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
newSubModuleClickedEvent = newSubModuleButton `mkMenuSignal` buttonPressEvent

renameModuleClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
renameModuleClickedEvent = renameModuleButton `mkMenuSignal` buttonPressEvent

deleteModuleClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
deleteModuleClickedEvent = deleteModuleButton `mkMenuSignal` buttonPressEvent

moveDeclarationClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
moveDeclarationClickedEvent = moveDeclarationButton `mkMenuSignal` buttonPressEvent

deleteDeclarationClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
deleteDeclarationClickedEvent = deleteDeclarationButton `mkMenuSignal` buttonPressEvent

editImportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
editImportClickedEvent = editImportButton `mkMenuSignal` buttonPressEvent

deleteImportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
deleteImportClickedEvent = deleteImportButton `mkMenuSignal` buttonPressEvent

editExportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
editExportClickedEvent = editExportButton `mkMenuSignal` buttonPressEvent

deleteExportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
deleteExportClickedEvent = deleteExportButton `mkMenuSignal` buttonPressEvent

newImportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
newImportClickedEvent = newImportButton `mkMenuSignal` buttonPressEvent

newExportClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
newExportClickedEvent = newExportButton `mkMenuSignal` buttonPressEvent

exportAllClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
exportAllClickedEvent = exportAllButton `mkMenuSignal` buttonPressEvent

newModuleClickedEvent :: ContextMenuSignal MenuItem (EventM EButton Bool)
newModuleClickedEvent = newModuleButton `mkMenuSignal` buttonPressEvent
