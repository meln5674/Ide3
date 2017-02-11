{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MainWindow.Menus
    ( makeMenus
    , module Dialogs.MainWindow.Menus.Signals
    , module Dialogs.MainWindow.Menus.Accelerators
    ) where

import Control.Monad.Trans

import GI.Gtk hiding (TreePath, SearchBar)

import GuiHelpers

import Dialogs.MainWindow.Menus.Types
import Dialogs.MainWindow.Menus.Signals
import Dialogs.MainWindow.Menus.Accelerators

makeMenus :: (MonadIO m) => MenuBar -> m Menus
makeMenus menuBar = do
    fileMenu <- makeFileMenu menuBar
    solutionMenu <- makeSolutionMenu menuBar
    searchMenu <- makeSearchMenu menuBar
    navigationMenu <- makeNavigationMenu menuBar
    projectMenu <- makeProjectMenu menuBar
    moduleMenu <- makeModuleMenu menuBar
    return Menus
        { fileMenu
        , solutionMenu
        , searchMenu
        , navigationMenu
        , projectMenu
        , moduleMenu
        }

makeFileMenu :: (MonadIO m) => MenuBar -> m FileMenu
makeFileMenu = makeFileMenuWith $ \fileMenu -> do
    newButton <- makeNewButton fileMenu
    openButton <- makeOpenButton fileMenu
    digestButton <- makeDigestButton fileMenu
    saveButton <- makeSaveButton fileMenu
    saveSolutionButton <- makeSaveSolutionButton fileMenu
    return FileMenu
           { newButton
           , openButton
           , digestButton
           , saveButton
           , saveSolutionButton
           }

makeFileMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: (MonadIO m) => Menu -> m MenuItem
makeNewButton = makeMenuButton "New Solution"

makeOpenButton :: (MonadIO m) => Menu -> m MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: (MonadIO m) => Menu -> m MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: (MonadIO m) => Menu -> m MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveSolutionButton :: (MonadIO m) => Menu -> m MenuItem
makeSaveSolutionButton = makeMenuButton "Save Solution"

 
makeSolutionMenu :: (MonadIO m) => MenuBar -> m SolutionMenu
makeSolutionMenu = makeSolutionMenuWith $ \solutionMenu -> do
    buildButton <- makeBuildButton solutionMenu
    runButton <- makeRunButton solutionMenu
    return SolutionMenu
           { buildButton
           , runButton
           }

makeSolutionMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeSolutionMenuWith = makeMenuWith "Solution"

makeBuildButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeRunButton = makeMenuButton "Run"

makeSearchMenu :: (MonadIO m) => MenuBar -> m SearchMenu
makeSearchMenu = makeSearchMenuWith $ \searchMenu -> do
    findButton <- makeFindButton searchMenu
    navigateButton <- makeNavigateButton searchMenu
    gotoDeclarationButton <- makeGotoDeclarationButton searchMenu
    return SearchMenu
        { findButton
        , navigateButton
        , gotoDeclarationButton
        }

makeSearchMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeSearchMenuWith = makeMenuWith "Search"

makeFindButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeFindButton = makeMenuButton "Find"

makeNavigateButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNavigateButton = makeMenuButton "Navigate"

makeGotoDeclarationButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeGotoDeclarationButton = makeMenuButton "Go to Declaration"

makeNavigationMenu :: (MonadIO m) => MenuBar -> m NavigationMenu
makeNavigationMenu = makeNavigationMenuWith $ \navigationMenu -> do
    backButton <- makeBackButton navigationMenu
    forwardButton <- makeForwardButton navigationMenu
    return NavigationMenu
        { backButton
        , forwardButton
        }

makeNavigationMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeNavigationMenuWith = makeMenuWith "Navigation"

makeBackButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeBackButton = makeMenuButton "Back"

makeForwardButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeForwardButton = makeMenuButton "Forward"

makeProjectMenu :: (MonadIO m) => MenuBar -> m ProjectMenu
makeProjectMenu = makeProjectMenuWith $ \projectMenu -> do
    newModuleButton <- makeNewModuleButton projectMenu
    deleteProjectButton <- makeDeleteProjectButton projectMenu
    return ProjectMenu 
        { newModuleButton
        , deleteProjectButton
        }

makeProjectMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeProjectMenuWith = makeMenuWith "Project"

makeNewModuleButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewModuleButton = makeMenuButton "New Module"

makeDeleteProjectButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeDeleteProjectButton = makeMenuButton "Delete"

makeModuleMenu :: (MonadIO m) => MenuBar -> m ModuleMenu
makeModuleMenu = makeModuleMenuWith $ \moduleMenu -> do
    newSubModuleButton <- makeNewSubModuleButton moduleMenu
    newPragmaButton <- makeNewPragmaButton moduleMenu
    newExportButton <- makeNewExportButton moduleMenu
    newImportButton <- makeNewImportButton moduleMenu
    newDeclarationButton <- makeNewDeclarationButton moduleMenu
    return ModuleMenu
        { newSubModuleButton
        , newPragmaButton
        , newExportButton
        , newImportButton
        , newDeclarationButton
        }

makeModuleMenuWith :: (MonadIO m) => (Menu -> m b) -> MenuBar -> m b
makeModuleMenuWith = makeMenuWith "Module"

makeNewSubModuleButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewSubModuleButton = makeMenuButton "New Sub Module"

makeNewPragmaButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewPragmaButton = makeMenuButton "New Pragma"

makeNewExportButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewExportButton = makeMenuButton "New Export"

makeNewImportButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewImportButton = makeMenuButton "New Import"

makeNewDeclarationButton :: (MonadIO m, IsMenuShell self) => self -> m MenuItem
makeNewDeclarationButton = makeMenuButton "New Declaration"
