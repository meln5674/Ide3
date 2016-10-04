module Dialogs.NewSolutionDialog.Types where

import Graphics.UI.Gtk

data NewSolutionDialog
    = NewSolutionDialog
    { window :: Window
    , fileChooser :: FileChooserWidget
    , projectNameBox :: Entry
    , projectNameBuffer :: EntryBuffer
    , templateNameBox :: Entry
    , templateNameBuffer :: EntryBuffer
    , projectNameLabel :: Label
    , templateNameLabel :: Label
    , confirmButton :: Button
    , cancelButton :: Button
    }
