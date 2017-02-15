{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MainWindow.Menus.Signals where

import GI.Gtk

import Dialogs.MainWindow.Signals

import Dialogs.MainWindow.Types

mkMenuSignal :: (Menus -> object) -> SignalProxy object info -> MainWindowSignal object info
mkMenuSignal getter = mkMainWindowSignal (getter . menus)

mkFileMenuSignal :: (FileMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkFileMenuSignal getter = mkMenuSignal (getter . fileMenu) 

mkSolutionMenuSignal :: (SolutionMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSolutionMenuSignal getter = mkMenuSignal (getter . solutionMenu)

mkSearchMenuSignal :: (SearchMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSearchMenuSignal getter = mkMenuSignal (getter . searchMenu)

mkNavigationMenuSignal :: (NavigationMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkNavigationMenuSignal getter = mkMenuSignal (getter . navigationMenu)

mkProjectMenuSignal :: (ProjectMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkProjectMenuSignal getter = mkMenuSignal (getter . projectMenu)

mkModuleMenuSignal :: (ModuleMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkModuleMenuSignal getter = mkMenuSignal (getter . moduleMenu)

newClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newClickedEvent = newButton `mkFileMenuSignal` #activate

openClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
openClickedEvent = openButton `mkFileMenuSignal` #activate

digestClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
digestClickedEvent = digestButton `mkFileMenuSignal` #activate

saveClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
saveClickedEvent = saveButton `mkFileMenuSignal` #activate

saveSolutionClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
saveSolutionClickedEvent = saveSolutionButton `mkFileMenuSignal` #activate

buildClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
buildClickedEvent = buildButton `mkSolutionMenuSignal` #activate

runClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
runClickedEvent = runButton `mkSolutionMenuSignal` #activate

runWithArgsClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
runWithArgsClickedEvent = runWithArgsButton `mkSolutionMenuSignal` #activate

findClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
findClickedEvent = findButton `mkSearchMenuSignal` #activate

navigateClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
navigateClickedEvent = navigateButton `mkSearchMenuSignal` #activate

gotoDeclarationClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
gotoDeclarationClickedEvent = gotoDeclarationButton `mkSearchMenuSignal` #activate

backClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
backClickedEvent = backButton `mkNavigationMenuSignal` #activate

forwardClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
forwardClickedEvent = forwardButton `mkNavigationMenuSignal` #activate

newModuleClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newModuleClickedEvent = newModuleButton `mkProjectMenuSignal` #activate

deleteProjectClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
deleteProjectClickedEvent = deleteProjectButton `mkProjectMenuSignal` #activate

newSubModuleClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newSubModuleClickedEvent = newSubModuleButton `mkModuleMenuSignal` #activate

newPragmaClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newPragmaClickedEvent = newPragmaButton `mkModuleMenuSignal` #activate

newExportClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newExportClickedEvent = newExportButton `mkModuleMenuSignal` #activate

newImportClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newImportClickedEvent = newImportButton `mkModuleMenuSignal` #activate

newDeclarationClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newDeclarationClickedEvent = newDeclarationButton `mkModuleMenuSignal` #activate
