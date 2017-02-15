module Dialogs.MainWindow.Menus.Types where

import GI.Gtk

data FileMenu
    = FileMenu
    { newButton :: MenuItem
    , openButton :: MenuItem
    , digestButton :: MenuItem
    , saveButton :: MenuItem
    , saveSolutionButton :: MenuItem
    }

data SolutionMenu
    = SolutionMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    , runWithArgsButton :: MenuItem
    }

data SearchMenu
    = SearchMenu
    { findButton :: MenuItem
    , navigateButton :: MenuItem
    , gotoDeclarationButton :: MenuItem
    }

data NavigationMenu
    = NavigationMenu
    { backButton :: MenuItem
    , forwardButton :: MenuItem
    }

data ProjectMenu
    = ProjectMenu
    { newModuleButton :: MenuItem
    , deleteProjectButton :: MenuItem
    }

data ModuleMenu
    = ModuleMenu
    { newSubModuleButton :: MenuItem
    , newPragmaButton :: MenuItem
    , newExportButton :: MenuItem
    , newImportButton :: MenuItem
    , newDeclarationButton :: MenuItem
    }

data Menus
    = Menus
    { fileMenu :: FileMenu
    , solutionMenu :: SolutionMenu
    , searchMenu :: SearchMenu
    , navigationMenu :: NavigationMenu
    , projectMenu :: ProjectMenu
    , moduleMenu :: ModuleMenu
    }
