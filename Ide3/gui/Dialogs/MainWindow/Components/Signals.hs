{-# LANGUAGE OverloadedLabels #-}
module Dialogs.MainWindow.Components.Signals where

import GI.Gtk

import Dialogs.MainWindow.Types
import Dialogs.MainWindow.Signals

mkSolutionViewerSignal :: (SolutionViewer -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSolutionViewerSignal getter = mkMainWindowSignal (getter . solutionViewer)

mkBuildViewerSignal :: (BuildViewer -> object) -> SignalProxy object info -> MainWindowSignal object info
mkBuildViewerSignal getter = mkMainWindowSignal (getter . buildViewer)

declClickedEvent :: MainWindowSignal TreeView TreeViewRowActivatedSignalInfo
declClickedEvent = projectView `mkSolutionViewerSignal` #rowActivated
    
projectViewClickedEvent :: MainWindowSignal TreeView WidgetButtonPressEventSignalInfo
projectViewClickedEvent = projectView `mkSolutionViewerSignal` #buttonPressEvent

errorClickedEvent :: MainWindowSignal TreeView TreeViewRowActivatedSignalInfo
errorClickedEvent = errorView `mkBuildViewerSignal` #rowActivated

windowClosedEvent :: MainWindowSignal Window WidgetDestroySignalInfo
windowClosedEvent = window `mkMainWindowSignal` #destroy

declarationEditedEvent :: MainWindowSignal TextBuffer TextBufferChangedSignalInfo
declarationEditedEvent = declBuffer `mkSolutionViewerSignal` #changed

solutionTreeQueryTooltipEvent :: MainWindowSignal TreeView WidgetQueryTooltipSignalInfo
solutionTreeQueryTooltipEvent = projectView `mkSolutionViewerSignal` #queryTooltip

{-
searchClickedEvent :: (Monad m)
                   => MainWindowSignal proxy m' p  m Button IO ()
searchClickedEvent = wrapGuiEnvSignal searchBar SearchBar.searchClickedEvent
-}



