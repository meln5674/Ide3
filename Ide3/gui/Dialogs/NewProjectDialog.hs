{-# LANGUAGE NamedFieldPuns #-}
module Dialogs.NewProjectDialog
    ( NewProjectDialog
    , NewProjectDialogSignal
    , make
    , close
    , confirmClicked
    , cancelClicked
    , getSelectedFolder
    , getProjectName
    , getTemplateName
    ) where

import Control.Monad
import Control.Monad.Trans

import System.Glib.UTFString


import Graphics.UI.Gtk

import GuiEnv
import GuiHelpers

data NewProjectDialog
    = NewProjectDialog
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

type NewProjectDialogSignal object handler = GuiSignal NewProjectDialog object handler

confirmClicked :: GuiSignal NewProjectDialog Button (EventM EButton Bool)
confirmClicked = confirmButton `mkGuiSignal` buttonPressEvent

cancelClicked :: GuiSignal NewProjectDialog Button (EventM EButton Bool)
cancelClicked = cancelButton `mkGuiSignal` buttonPressEvent

getSelectedFolder :: (MonadIO m) => NewProjectDialog -> m (Maybe FilePath)
getSelectedFolder = liftIO . fileChooserGetFilename . fileChooser

getProjectName :: (MonadIO m, GlibString string) => NewProjectDialog -> m string
getProjectName = liftIO . flip get entryBufferText . projectNameBuffer

getTemplateName :: MonadIO m => NewProjectDialog -> m (Maybe String)
getTemplateName = liftIO . liftM f . flip get entryBufferText . templateNameBuffer
  where
    f "" = Nothing
    f x = Just x

close :: MonadIO m => NewProjectDialog -> m ()
close = liftIO . widgetDestroy . window

makeVBoxWith :: (MonadIO m, ContainerClass self) => self -> (VBox -> m b) -> m b
makeVBoxWith window f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ window `containerAdd` vbox
    f vbox

makeFileChooser :: (MonadIO m, BoxClass self) => self -> m FileChooserWidget
makeFileChooser vbox = liftIO $ do
    fileChooser <- fileChooserWidgetNew FileChooserActionSelectFolder
    boxPackStart vbox fileChooser PackGrow 0 
    return fileChooser

makeHBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b
makeHBoxWith vbox f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ boxPackEnd vbox hbox PackNatural 0
    f hbox

makeProjectBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeProjectBoxWith = makeHBoxWith

makeTemplateBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeTemplateBoxWith = makeHBoxWith

makeButtonBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeButtonBoxWith = makeHBoxWith

makeProjectNameLabel :: (MonadIO m, BoxClass self) => self -> m Label
makeProjectNameLabel hbox = liftIO $ do
    projectNameLabel <- liftIO $ labelNew (Just "Project Name")
    boxPackStart hbox projectNameLabel PackNatural 0
    return projectNameLabel

makeProjectNameBox :: (MonadIO m, EntryBufferClass buffer, BoxClass self) 
                   => self -> buffer -> m Entry
makeProjectNameBox hbox buffer = liftIO $ do
    projectNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox projectNameBox PackGrow 0
    return projectNameBox

makeTemplateNameLabel :: (MonadIO m, BoxClass self) => self -> m Label
makeTemplateNameLabel hbox = liftIO $ do
    templateNameLabel <- labelNew (Just "Template Name (Optional)")
    boxPackStart hbox templateNameLabel PackNatural 0
    return templateNameLabel

makeTemplateNameBox :: (MonadIO m, EntryBufferClass buffer, BoxClass self) 
                    => self -> buffer -> m Entry
makeTemplateNameBox hbox buffer = liftIO $ do
    templateNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox templateNameBox PackGrow 0
    return templateNameBox

makeConfirmButton :: (MonadIO m, BoxClass self) => self -> m Button
makeConfirmButton hbox = liftIO $ do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackEnd hbox confirmButton PackGrow 0
    return confirmButton

makeCancelButton :: (MonadIO m, BoxClass self) => self -> m Button
makeCancelButton hbox = liftIO $ do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton PackGrow 0
    return cancelButton

make :: (MonadIO m) => (NewProjectDialog -> m b) -> m b
make f = makeWindowWith
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        fileChooser <- makeFileChooser vbox
        projectNameBuffer <- liftIO $ entryBufferNew (Nothing :: Maybe String)
        templateNameBuffer <- liftIO $ entryBufferNew (Nothing :: Maybe String)
        (projectNameBox, projectNameLabel) <- makeProjectBoxWith vbox $ \hbox -> do
            projectNameLabel <- makeProjectNameLabel hbox
            projectNameBox <- makeProjectNameBox hbox projectNameBuffer
            return (projectNameBox, projectNameLabel)
        (templateNameBox, templateNameLabel) <- makeTemplateBoxWith vbox $ \hbox -> do
            templateNameLabel <- makeTemplateNameLabel hbox
            templateNameBox <- makeTemplateNameBox hbox templateNameBuffer
            return (templateNameBox, templateNameLabel)
        (confirmButton, cancelButton) <- makeButtonBoxWith vbox $ \hbox -> do
            confirmButton <- makeConfirmButton hbox
            cancelButton <- makeCancelButton hbox
            return (confirmButton,cancelButton)
        f NewProjectDialog
          { window
          , fileChooser
          , projectNameBox
          , projectNameBuffer
          , templateNameBox
          , templateNameBuffer
          , projectNameLabel
          , templateNameLabel
          , confirmButton
          , cancelButton
          }
    
