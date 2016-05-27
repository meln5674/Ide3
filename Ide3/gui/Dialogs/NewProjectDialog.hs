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

import System.Glib.UTFString

import Control.Monad

import Graphics.UI.Gtk

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

type NewProjectDialogSignal = GuiSignal NewProjectDialog

confirmClicked :: GuiSignal NewProjectDialog Button (EventM EButton Bool)
confirmClicked = confirmButton `mkGuiSignal` buttonPressEvent

cancelClicked :: GuiSignal NewProjectDialog Button (EventM EButton Bool)
cancelClicked = cancelButton `mkGuiSignal` buttonPressEvent

getSelectedFolder :: NewProjectDialog -> IO (Maybe FilePath)
getSelectedFolder = fileChooserGetFilename . fileChooser

getProjectName :: (GlibString string) => NewProjectDialog -> IO string
getProjectName = flip get entryBufferText . projectNameBuffer

getTemplateName :: NewProjectDialog -> IO (Maybe String)
getTemplateName = liftM f . flip get entryBufferText . templateNameBuffer
  where
    f "" = Nothing
    f x = Just x

close :: NewProjectDialog -> IO ()
close = widgetDestroy . window

makeWindowWith :: (Window -> IO b) -> IO b
makeWindowWith f = do
    window <- windowNew
    windowSetModal window True
    r <- f window
    widgetShowAll window
    return r

makeVBoxWith :: ContainerClass self => self -> (VBox -> IO b) -> IO b
makeVBoxWith window f = do
    vbox <- vBoxNew False 0
    window `containerAdd` vbox
    f vbox

makeFileChooser :: BoxClass self => self -> IO FileChooserWidget
makeFileChooser vbox = do
    fileChooser <- fileChooserWidgetNew FileChooserActionSelectFolder
    boxPackStart vbox fileChooser PackGrow 0 
    return fileChooser

makeHBoxWith :: BoxClass self => self -> (HBox -> IO b) -> IO b
makeHBoxWith vbox f = do
    hbox <- hBoxNew False 0
    boxPackEnd vbox hbox PackNatural 0
    f hbox

makeProjectBoxWith :: BoxClass self => self -> (HBox -> IO b) -> IO b    
makeProjectBoxWith = makeHBoxWith

makeTemplateBoxWith :: BoxClass self => self -> (HBox -> IO b) -> IO b    
makeTemplateBoxWith = makeHBoxWith

makeButtonBoxWith :: BoxClass self => self -> (HBox -> IO b) -> IO b    
makeButtonBoxWith = makeHBoxWith

makeProjectNameLabel :: BoxClass self => self -> IO Label
makeProjectNameLabel hbox = do
    projectNameLabel <- labelNew (Just "Project Name")
    boxPackStart hbox projectNameLabel PackNatural 0
    return projectNameLabel

makeProjectNameBox :: (EntryBufferClass buffer, BoxClass self) 
                   => self -> buffer -> IO Entry
makeProjectNameBox hbox buffer = do
    projectNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox projectNameBox PackGrow 0
    return projectNameBox

makeTemplateNameLabel :: BoxClass self => self -> IO Label
makeTemplateNameLabel hbox = do
    templateNameLabel <- labelNew (Just "Template Name (Optional)")
    boxPackStart hbox templateNameLabel PackNatural 0
    return templateNameLabel

makeTemplateNameBox :: (EntryBufferClass buffer, BoxClass self) 
                    => self -> buffer -> IO Entry
makeTemplateNameBox hbox buffer = do
    templateNameBox <- entryNewWithBuffer buffer
    boxPackEnd hbox templateNameBox PackGrow 0
    return templateNameBox

makeConfirmButton :: BoxClass self => self -> IO Button
makeConfirmButton hbox = do
    confirmButton <- buttonNewWithLabel "Confirm"
    boxPackEnd hbox confirmButton PackGrow 0
    return confirmButton

makeCancelButton :: BoxClass self => self -> IO Button
makeCancelButton hbox = do
    cancelButton <- buttonNewWithLabel "Cancel"
    boxPackEnd hbox cancelButton PackGrow 0
    return cancelButton

make :: (NewProjectDialog -> IO b) -> IO b
make f = makeWindowWith 
    $ \window -> makeVBoxWith window
    $ \vbox -> do
        fileChooser <- makeFileChooser vbox
        projectNameBuffer <- entryBufferNew (Nothing :: Maybe String)
        templateNameBuffer <- entryBufferNew (Nothing :: Maybe String)
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
            
    
