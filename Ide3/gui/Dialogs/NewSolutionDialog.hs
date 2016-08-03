{-# LANGUAGE NamedFieldPuns #-}
module Dialogs.NewSolutionDialog
    ( NewSolutionDialog
    , NewSolutionDialogSignal
    , make
    , close
    , confirmClicked
    , cancelClicked
    , getSelectedFolder
    , getSolutionName
    , getTemplateName
    ) where

import Control.Monad
import Control.Monad.Trans

import System.Glib.UTFString


import Graphics.UI.Gtk

import GuiEnv
import GuiHelpers

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

type NewSolutionDialogSignal object handler = GuiSignal NewSolutionDialog object handler

confirmClicked :: GuiSignal NewSolutionDialog Button (EventM EButton Bool)
confirmClicked = confirmButton `mkGuiSignal` buttonPressEvent

cancelClicked :: GuiSignal NewSolutionDialog Button (EventM EButton Bool)
cancelClicked = cancelButton `mkGuiSignal` buttonPressEvent

getSelectedFolder :: (MonadIO m) => NewSolutionDialog -> m (Maybe FilePath)
getSelectedFolder = liftIO . fileChooserGetFilename . fileChooser

getSolutionName :: (MonadIO m, GlibString string) => NewSolutionDialog -> m string
getSolutionName = liftIO . flip get entryBufferText . projectNameBuffer

getTemplateName :: MonadIO m => NewSolutionDialog -> m (Maybe String)
getTemplateName = liftIO . liftM f . flip get entryBufferText . templateNameBuffer
  where
    f "" = Nothing
    f x = Just x

close :: MonadIO m => NewSolutionDialog -> m ()
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

makeSolutionBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeSolutionBoxWith = makeHBoxWith

makeTemplateBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeTemplateBoxWith = makeHBoxWith

makeButtonBoxWith :: (MonadIO m, BoxClass self) => self -> (HBox -> m b) -> m b    
makeButtonBoxWith = makeHBoxWith

makeSolutionNameLabel :: (MonadIO m, BoxClass self) => self -> m Label
makeSolutionNameLabel hbox = liftIO $ do
    projectNameLabel <- liftIO $ labelNew (Just "Solution Name")
    boxPackStart hbox projectNameLabel PackNatural 0
    return projectNameLabel

makeSolutionNameBox :: (MonadIO m, EntryBufferClass buffer, BoxClass self) 
                   => self -> buffer -> m Entry
makeSolutionNameBox hbox buffer = liftIO $ do
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

make :: (MonadIO m) => (NewSolutionDialog -> m b) -> m b
make f = makeWindowWith
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        fileChooser <- makeFileChooser vbox
        projectNameBuffer <- liftIO $ entryBufferNew (Nothing :: Maybe String)
        templateNameBuffer <- liftIO $ entryBufferNew (Nothing :: Maybe String)
        (projectNameBox, projectNameLabel) <- makeSolutionBoxWith vbox $ \hbox -> do
            projectNameLabel <- makeSolutionNameLabel hbox
            projectNameBox <- makeSolutionNameBox hbox projectNameBuffer
            return (projectNameBox, projectNameLabel)
        (templateNameBox, templateNameLabel) <- makeTemplateBoxWith vbox $ \hbox -> do
            templateNameLabel <- makeTemplateNameLabel hbox
            templateNameBox <- makeTemplateNameBox hbox templateNameBuffer
            return (templateNameBox, templateNameLabel)
        (confirmButton, cancelButton) <- makeButtonBoxWith vbox $ \hbox -> do
            confirmButton <- makeConfirmButton hbox
            cancelButton <- makeCancelButton hbox
            return (confirmButton,cancelButton)
        f NewSolutionDialog
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
    
