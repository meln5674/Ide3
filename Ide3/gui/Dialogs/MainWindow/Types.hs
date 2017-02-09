module Dialogs.MainWindow.Types 
    ( MainWindow (..)
    , SolutionPathCoords (..)
    , FocusTarget (..)
    , module Dialogs.MainWindow.Menus.Types
    , module Dialogs.MainWindow.Components.Types
    ) where

import GI.Gtk hiding (SearchBar)

import Dialogs.SearchBar

import Dialogs.MainWindow.Menus.Types
import Dialogs.MainWindow.Components.Types

-- | ADT for the main application window
data MainWindow
    = MainWindow
    { window :: Window
    , fileMenu :: FileMenu
    , solutionMenu :: SolutionMenu
    , searchMenu :: SearchMenu
    , navigationMenu :: NavigationMenu
    , projectMenu :: ProjectMenu
    , moduleMenu :: ModuleMenu
    , solutionViewer :: SolutionViewer
    , buildViewer :: BuildViewer
    , searchBar :: SearchBar
    }  


data SolutionPathCoords
    = BinWindowCoords Int Int
    | WidgetCoords Int Int

data FocusTarget
    = FocusEditor
    | FocusLog
    | FocusErrorList
    | FocusSolutionTree


