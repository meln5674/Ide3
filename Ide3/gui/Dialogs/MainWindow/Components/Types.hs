module Dialogs.MainWindow.Components.Types where

import GI.Gtk

import BetterTextView

data SolutionViewer
    = SolutionViewer
    { projectView :: TreeView
    , declView :: BetterTextView
    , declBuffer :: TextBuffer
    }

data BuildViewer
    = BuildViewer
    { buildView :: TextView
    , errorView :: TreeView
    }


