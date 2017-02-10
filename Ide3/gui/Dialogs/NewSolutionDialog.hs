{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Dialogs.NewSolutionDialog
    ( NewSolutionDialog
    , NewSolutionDialogSignal
    , make
    , close
    , setVisible
    , confirmClicked
    , cancelClicked
    , getSelectedFolder
    , getSolutionName
    , getTemplateName
    ) where

import Data.Text

import Control.Monad
import Control.Monad.Trans

import GI.Gtk

import GuiHelpers

import Dialogs.NewSolutionDialog.Types

type NewSolutionDialogSignal subObject info = SubSignalProxy NewSolutionDialog subObject info

confirmClicked :: NewSolutionDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClicked dialog = (confirmButton dialog, #buttonPressEvent) 

cancelClicked :: NewSolutionDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClicked dialog = (cancelButton dialog, #buttonPressEvent)

getSelectedFolder :: (MonadIO m) => NewSolutionDialog -> m (Maybe FilePath)
getSelectedFolder = liftIO . fileChooserGetFilename . fileChooser

getSolutionName :: (MonadIO m) => NewSolutionDialog -> m Text
getSolutionName = liftIO . flip get entryBufferText . projectNameBuffer

getTemplateName :: MonadIO m => NewSolutionDialog -> m (Maybe Text)
getTemplateName = liftIO . liftM f . flip get entryBufferText . templateNameBuffer
  where
    f "" = Nothing
    f x = Just x

close :: MonadIO m => NewSolutionDialog -> m ()
close = liftIO . widgetDestroy . window

setVisible :: MonadIO m => NewSolutionDialog -> Bool -> m ()
setVisible dialog v = set (window dialog) [widgetVisible := v]

{-
makeVBoxWith :: (MonadIO m, ContainerClass self) => self -> (VBox -> m b) -> m b
makeVBoxWith window f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ window `containerAdd` vbox
    f vbox
-}

makeFileChooser :: (MonadIO m, IsBox self) => self -> m FileChooserWidget
makeFileChooser vbox = liftIO $ do
    fileChooser <- fileChooserWidgetNew FileChooserActionSelectFolder
    boxPackStart vbox fileChooser True True 0 
    return fileChooser

{-
makeHBoxWith :: (MonadIO m, IsBox self) => self -> (HBox -> m b) -> m b
makeHBoxWith vbox f = do
    hbox <- liftIO $ hBoxNew False 0
    liftIO $ boxPackEnd vbox hbox False False 0
    f hbox
-}

makeSolutionBoxWith :: (MonadIO m, IsContainer self) => self -> (Box -> m b) -> m b    
makeSolutionBoxWith = makeHBoxWith

makeTemplateBoxWith :: (MonadIO m, IsContainer self) => self -> (Box -> m b) -> m b    
makeTemplateBoxWith = makeHBoxWith

makeButtonBoxWith :: (MonadIO m, IsContainer self) => self -> (Box -> m b) -> m b    
makeButtonBoxWith = makeHBoxWith

makeSolutionNameLabel :: (MonadIO m, IsBox self) => self -> m Label
makeSolutionNameLabel hbox = liftIO $ do
    projectNameLabel <- liftIO $ labelNew (Just "Solution Name")
    boxPackStart hbox projectNameLabel False False 0
    return projectNameLabel

makeSolutionNameBox :: (MonadIO m, IsEntryBuffer buffer, IsBox self) 
                   => self -> buffer -> m Entry
makeSolutionNameBox hbox buffer = liftIO $ do
    projectNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox projectNameBox True True 0
    return projectNameBox

makeTemplateNameLabel :: (MonadIO m, IsBox self) => self -> m Label
makeTemplateNameLabel hbox = liftIO $ do
    templateNameLabel <- labelNew (Just "Template Name (Optional)")
    boxPackStart hbox templateNameLabel False False 0
    return templateNameLabel

makeTemplateNameBox :: (MonadIO m, IsEntryBuffer buffer, IsBox self) 
                    => self -> buffer -> m Entry
makeTemplateNameBox hbox buffer = liftIO $ do
    templateNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox templateNameBox True True 0
    return templateNameBox

makeConfirmButton :: (MonadIO m, IsBox self) => self -> m Button
makeConfirmButton hbox = liftIO $ do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackEnd hbox confirmButton True True 0
    return confirmButton

makeCancelButton :: (MonadIO m, IsBox self) => self -> m Button
makeCancelButton hbox = liftIO $ do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton True True 0
    return cancelButton

make :: (MonadIO m) => (NewSolutionDialog -> m b) -> m b
make f = makeWindowWith
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        fileChooser <- makeFileChooser vbox
        projectNameBuffer <- entryBufferNew Nothing 0
        templateNameBuffer <- entryBufferNew Nothing 0
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
        void $ window `GI.Gtk.on` #deleteEvent $ \_ -> widgetHideOnDelete window
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
    
