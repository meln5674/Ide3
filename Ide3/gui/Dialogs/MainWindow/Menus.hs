{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MainWindow.Menus
    ( makeFileMenu
    , makeSolutionMenu
    , makeSearchMenu
    , makeNavigationMenu
    , makeProjectMenu
    , makeModuleMenu
    , module Dialogs.MainWindow.Menus.Signals
    , module Dialogs.MainWindow.Menus.Accelerators
    ) where

import Data.Monoid

import Data.Text hiding (map)

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk hiding (TreePath, SearchBar)
import GI.Gdk hiding (Window, windowNew)
import Data.GI.Gtk.ModelView.CellLayout

import Ide3.Types

import BetterTextView

import ErrorParser.Types

import GuiClass.Types

import GuiEnv
import GuiMonad

import GuiHelpers

import Dialogs.SearchBar (SearchBar)
import qualified Dialogs.SearchBar as SearchBar

import SearchMode
import DeclarationPath

import GuiClass (SolutionTreeElem (..))

import Dialogs.MainWindow.Menus.Types
import Dialogs.MainWindow.Menus.Signals
import Dialogs.MainWindow.Menus.Accelerators

makeFileMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m FileMenu
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

makeFileMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeNewButton = makeMenuButton "New Solution"

makeOpenButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveSolutionButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeSaveSolutionButton = makeMenuButton "Save Solution"

 
makeSolutionMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m SolutionMenu
makeSolutionMenu = makeSolutionMenuWith $ \solutionMenu -> do
    buildButton <- makeBuildButton solutionMenu
    runButton <- makeRunButton solutionMenu
    return SolutionMenu
           { buildButton
           , runButton
           }

makeSolutionMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeSolutionMenuWith = makeMenuWith "Solution"

makeBuildButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeRunButton = makeMenuButton "Run"

makeSearchMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m SearchMenu
makeSearchMenu = makeSearchMenuWith $ \searchMenu -> do
    findButton <- makeFindButton searchMenu
    navigateButton <- makeNavigateButton searchMenu
    gotoDeclarationButton <- makeGotoDeclarationButton searchMenu
    return SearchMenu
        { findButton
        , navigateButton
        , gotoDeclarationButton
        }

makeSearchMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeSearchMenuWith = makeMenuWith "Search"

makeFindButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeFindButton = makeMenuButton "Find"

makeNavigateButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeNavigateButton = makeMenuButton "Navigate"

makeGotoDeclarationButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeGotoDeclarationButton = makeMenuButton "Go to Declaration"

makeNavigationMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p m NavigationMenu
makeNavigationMenu = makeNavigationMenuWith $ \navigationMenu -> do
    backButton <- makeBackButton navigationMenu
    forwardButton <- makeForwardButton navigationMenu
    return NavigationMenu
        { backButton
        , forwardButton
        }

makeNavigationMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p m b) -> MenuBar -> GuiEnvT {-proxy-} m' p m b
makeNavigationMenuWith = makeMenuWith "Navigation"

makeBackButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBackButton = makeMenuButton "Back"

makeForwardButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeForwardButton = makeMenuButton "Forward"

makeProjectMenu :: (MonadIO m) => MenuBar -> GuiEnvT m' p m ProjectMenu
makeProjectMenu = makeProjectMenuWith $ \projectMenu -> do
    newModuleButton <- makeNewModuleButton projectMenu
    deleteProjectButton <- makeDeleteProjectButton projectMenu
    return ProjectMenu 
        { newModuleButton
        , deleteProjectButton
        }

makeProjectMenuWith :: (MonadIO m) => (Menu -> GuiEnvT m' p m b) -> MenuBar -> GuiEnvT m' p m b
makeProjectMenuWith = makeMenuWith "Project"

makeNewModuleButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewModuleButton = makeMenuButton "New Module"

makeDeleteProjectButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeDeleteProjectButton = makeMenuButton "Delete"

makeModuleMenu :: (MonadIO m) => MenuBar -> GuiEnvT m' p m ModuleMenu
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

makeModuleMenuWith :: (MonadIO m) => (Menu -> GuiEnvT m' p m b) -> MenuBar -> GuiEnvT m' p m b
makeModuleMenuWith = makeMenuWith "Module"

makeNewSubModuleButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewSubModuleButton = makeMenuButton "New Sub Module"

makeNewPragmaButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewPragmaButton = makeMenuButton "New Pragma"

makeNewExportButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewExportButton = makeMenuButton "New Export"

makeNewImportButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewImportButton = makeMenuButton "New Import"

makeNewDeclarationButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT m' p m MenuItem
makeNewDeclarationButton = makeMenuButton "New Declaration"
