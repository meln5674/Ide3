module Dialogs.NewSolutionDialog.Types where

import qualified GI.Gtk as Gtk
data NewSolutionDialog
    = NewSolutionDialog
    { window :: Gtk.Window
    , fileChooser :: Gtk.FileChooserWidget
    , projectNameBox :: Gtk.Entry
    , projectNameBuffer :: Gtk.EntryBuffer
    , templateNameBox :: Gtk.Entry
    , templateNameBuffer :: Gtk.EntryBuffer
    , projectNameLabel :: Gtk.Label
    , templateNameLabel :: Gtk.Label
    , confirmButton :: Gtk.Button
    , cancelButton :: Gtk.Button
    }
