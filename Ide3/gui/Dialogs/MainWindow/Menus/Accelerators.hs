{-# LANGUAGE OverloadedStrings #-}
module Dialogs.MainWindow.Menus.Accelerators where

import Data.Text (Text)

import Control.Monad.Trans

import GI.Gdk
import GI.Gtk

import GuiHelpers

import Dialogs.MainWindow.Types
import Dialogs.MainWindow.Accelerators

addMenusAccelerator :: ( MonadIO m
                          , IsAccelGroup group
                          , Integral key
                          , IsWidget subObject
                          )
                       => (Menus -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addMenusAccelerator f = addMainWindowAccelerator (f . menus)

addFileMenuAccelerator :: ( MonadIO m
                          , IsAccelGroup group
                          , Integral key
                          , IsWidget subObject
                          )
                       => (FileMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addFileMenuAccelerator f = addMenusAccelerator (f . fileMenu)

addSolutionMenuAccelerator :: ( MonadIO m
                              , IsAccelGroup group
                              , Integral key
                              , IsWidget subObject
                              )
                       => (SolutionMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addSolutionMenuAccelerator f = addMenusAccelerator (f . solutionMenu)

addSearchMenuAccelerator :: ( MonadIO m
                            , IsAccelGroup group
                            , Integral key
                            , IsWidget subObject
                            )
                       => (SearchMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addSearchMenuAccelerator f = addMenusAccelerator (f . searchMenu)

addNavigationMenuAccelerator :: ( MonadIO m
                                , IsAccelGroup group
                                , Integral key
                                , IsWidget subObject
                                )
                       => (NavigationMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addNavigationMenuAccelerator f = addMenusAccelerator (f . navigationMenu)

addNewClickedEventAccelerator :: ( MonadIO m
                                 , IsAccelGroup group
                                 , Integral key
                                 )
                              => MainWindow
                              -> group
                              -> key
                              -> [ModifierType] 
                              -> [AccelFlags]
                              -> m ()
addNewClickedEventAccelerator = newButton `addFileMenuAccelerator` "activate"

addOpenClickedEventAccelerator :: ( MonadIO m
                                  , IsAccelGroup group
                                  , Integral key
                                  )
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addOpenClickedEventAccelerator = openButton `addFileMenuAccelerator` "activate"

addDigestClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addDigestClickedEventAccelerator = digestButton `addFileMenuAccelerator` "activate"

addSaveClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addSaveClickedEventAccelerator = saveButton `addFileMenuAccelerator` "activate"

addSaveSolutionClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                 -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addSaveSolutionClickedEventAccelerator = saveSolutionButton `addFileMenuAccelerator` "activate"

addBuildClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addBuildClickedEventAccelerator = buildButton `addSolutionMenuAccelerator` "activate"

addRunClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addRunClickedEventAccelerator = runButton `addSolutionMenuAccelerator` "activate"

--addFindClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addFindClickedEventAccelerator = findButton `addSearchMenuAccelerator` "activate"

--addNavigateClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addNavigateClickedEventAccelerator = navigateButton `addSearchMenuAccelerator` "activate"

--addSearchClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addSearchClickedEventAccelerator = SearchBar.addSearchClickedEventAccelerator . searchBar

addGotoDeclarationEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addGotoDeclarationEventAccelerator = gotoDeclarationButton `addSearchMenuAccelerator` "activate"

addBackEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addBackEventAccelerator = backButton `addNavigationMenuAccelerator` "activate"

addForwardEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addForwardEventAccelerator = forwardButton `addNavigationMenuAccelerator` "activate"



