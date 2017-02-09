module Dialogs.MainWindow.Signals where

import GI.Gtk

import GuiHelpers

import Dialogs.MainWindow.Types

type MainWindowSignal object info = SubSignalProxy MainWindow object info

mkMainWindowSignal :: (MainWindow -> object) -> SignalProxy object info -> MainWindowSignal object info
mkMainWindowSignal getter signal window = (getter window, signal)


