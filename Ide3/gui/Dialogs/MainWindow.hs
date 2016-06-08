{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{- LANGUAGE PartialTypeSignatures -}
module Dialogs.MainWindow
    ( MainWindow
    , make
    , newClickedEvent
    , openClickedEvent
    , digestClickedEvent
    , declClickedEvent
    , projectViewClickedEvent
    , saveClickedEvent
    , saveProjectClickedEvent
    , buildClickedEvent
    , runClickedEvent
    , windowClosedEvent
    , getProjectPathClicked
    , addAccelGroup
    , addNewClickedEventAccelerator
    , addOpenClickedEventAccelerator
    , addDigestClickedEventAccelerator
    , addSaveClickedEventAccelerator
    , addSaveProjectClickedEventAccelerator
    , addBuildClickedEventAccelerator
    , addRunClickedEventAccelerator
    ) where

import Data.Text

import Data.Functor.Compose

import Control.Monad.Trans

import Graphics.UI.Gtk


import GuiEnv
import GuiMonad

import ProjectTree

import GuiHelpers

data MainWindow
    = MainWindow
    { window :: Window
    , fileMenu :: FileMenu
    , projectMenu :: ProjectMenu
    , projectViewer :: ProjectViewer
    , buildViewer :: BuildViewer
    }  


--make :: GuiEnv proxy m p buffer -> (MainWindow -> m a) -> m ()

make :: (TextBufferClass buffer, MonadIO m)
     => (MainWindow -> GuiEnvT proxy m' p buffer m a) 
     -> GuiEnvT proxy m' p buffer m a
make f = makeMainWindowWith $ \window -> 
    makeVBoxWith window $ \vbox -> 
        makeContainerWith vbox $ \container -> do
            renderer <- makeRenderer
            menuBar <- makeMainMenuBar vbox
            
            fileMenu <- makeFileMenu menuBar
            projectMenu <- makeProjectMenu menuBar
            projectViewer <- makeProjectViewer renderer 
                                               renderProjectTreeElem
                                               container
            buildViewer <- makeBuildViewer container
            f MainWindow
              { window
              , fileMenu
              , projectMenu
              , projectViewer
              , buildViewer
              }

makeRenderer :: (MonadIO m) => GuiEnvT proxy m' p buffer m CellRendererText
makeRenderer = liftIO cellRendererTextNew

makeVBoxWith :: (MonadIO m, ContainerClass self) => self -> (VBox -> GuiEnvT proxy m' p buffer m b) -> GuiEnvT proxy m' p buffer m b
makeVBoxWith window f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ window `containerAdd` vbox
    f vbox

makeContainerWith :: (MonadIO m, BoxClass self) => self -> (Table -> GuiEnvT proxy m' p buffer m b) -> GuiEnvT proxy m' p buffer m b
makeContainerWith vbox f = do
    container <- liftIO $ tableNew 2 3 False
    liftIO $ boxPackEnd vbox container PackGrow 0
    f container

makeMainMenuBar :: (MonadIO m, BoxClass self) => self -> GuiEnvT proxy m' p buffer m MenuBar
makeMainMenuBar vbox = do
    menuBar <- liftIO $ menuBarNew
    liftIO $ boxPackStart vbox menuBar PackNatural 0
    return menuBar

data FileMenu
    = FileMenu
    { newButton :: MenuItem
    , openButton :: MenuItem
    , digestButton :: MenuItem
    , saveButton :: MenuItem
    , saveProjectButton :: MenuItem
    }

makeFileMenu :: (MonadIO m) => MenuBar -> GuiEnvT proxy m' p buffer m FileMenu
makeFileMenu = makeFileMenuWith $ \fileMenu -> do
    newButton <- makeNewButton fileMenu
    openButton <- makeOpenButton fileMenu
    digestButton <- makeDigestButton fileMenu
    saveButton <- makeSaveButton fileMenu
    saveProjectButton <- makeSaveProjectButton fileMenu
    return FileMenu
           { newButton
           , openButton
           , digestButton
           , saveButton
           , saveProjectButton
           }

makeFileMenuWith :: (MonadIO m) => (Menu -> GuiEnvT proxy m' p buffer m b) -> MenuBar -> GuiEnvT proxy m' p buffer m b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeNewButton = makeMenuButton "New Project"

makeOpenButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveProjectButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeSaveProjectButton = makeMenuButton "Save Project"

 
data ProjectMenu
    = ProjectMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    }

makeProjectMenu :: (MonadIO m) => MenuBar -> GuiEnvT proxy m' p buffer m ProjectMenu
makeProjectMenu = makeProjectMenuWith $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return ProjectMenu
           { buildButton
           , runButton
           }

makeProjectMenuWith :: (MonadIO m) => (Menu -> GuiEnvT proxy m' p buffer m b) -> MenuBar -> GuiEnvT proxy m' p buffer m b
makeProjectMenuWith = makeMenuWith "Project"

makeBuildButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT proxy m' p buffer m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT proxy m' p buffer m MenuItem
makeRunButton = makeMenuButton "Run"

data ProjectViewer
    = ProjectViewer
    { projectView :: TreeView
    , declView :: TextView
    }

makeProjectViewer :: (MonadIO m
                     , TableClass self
                     , CellRendererClass cell
                     , TextBufferClass buffer
                     ) 
                  => cell
                  -> (ProjectTreeElem -> [AttrOp cell])
                  -> self
                  -> GuiEnvT proxy m' p buffer m ProjectViewer
makeProjectViewer renderer renderFunc container = do
    projectView <- makeProjView renderer container renderFunc
    declView <- makeDeclView container
    return ProjectViewer
           { projectView
           , declView
           }

makeProjView :: ( MonadIO m
                , TableClass self
                , CellRendererClass cell
                , TextBufferClass buffer
                ) 
             => cell 
             -> self 
             -> (ProjectTreeElem -> [AttrOp cell]) 
             -> GuiEnvT proxy m' p buffer m TreeView
makeProjView renderer container renderFunc = do
    treeViewColumn <- liftIO treeViewColumnNew
    projView <- withGuiComponents $ flip withProjectTree $ liftIO . treeViewNewWithModel
    _ <- liftIO $ treeViewAppendColumn projView treeViewColumn
    liftIO $ cellLayoutPackStart treeViewColumn renderer False
    withGuiComponents $ flip withProjectTree $ \treeStore -> 
        liftIO $ cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
    scrollWindow <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ scrollWindow `containerAdd` projView
    liftIO $ tableAttach
        container
        scrollWindow
        0 1
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return projView

makeDeclView :: ( MonadIO m
                , TableClass self
                , TextBufferClass buffer
                ) 
             => self -> GuiEnvT proxy m' p buffer m TextView
makeDeclView container = do
    declView <- withGuiComponents $ flip withEditorBuffer $ liftIO . textViewNewWithBuffer
    monospace <- liftIO fontDescriptionNew
    liftIO $ monospace `fontDescriptionSetFamily` "monospace"
    liftIO $ declView `widgetModifyFont` Just monospace
    scrollWindow <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ scrollWindow `containerAdd` declView
    liftIO $ tableAttach
        container
        scrollWindow
        1 2
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return declView

data BuildViewer
    = BuildViewer
    { buildView :: TextView
    }

makeBuildViewer :: ( MonadIO m
                   , TextBufferClass buffer
                   , TableClass self
                   ) 
                => self -> GuiEnvT proxy m' p buffer m BuildViewer
makeBuildViewer container = do
    buildView <- makeBuildView container
    return BuildViewer
           { buildView
           }

makeBuildView :: ( MonadIO m
                 , TextBufferClass buffer
                 , TableClass self
                 ) 
              => self -> GuiEnvT proxy m' p buffer m TextView
makeBuildView container = do
    buildView <- withGuiComponents $ flip withBuildBuffer $ liftIO . textViewNewWithBuffer
    liftIO $ tableAttach
        container
        buildView
        0 2
        2 3
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return buildView

makeMainWindowWith :: (MonadIO m) => (Window -> GuiEnvT proxy m' p buffer m a) -> GuiEnvT proxy m' p buffer m a
makeMainWindowWith f = do
    window <- liftIO $ do
        _ <- initGUI
        windowNew
    r <- f window
    liftIO $ do
        widgetShowAll window
        mainGUI
    return r


type MainWindowSignal proxy m' p buffer m object m'' a
    = GuiEnvSignal proxy m' p buffer m MainWindow object m'' a

type MainWindowSignal2 proxy m' p buffer m object f m'' a
    = GuiEnvSignal2 proxy m' p buffer m MainWindow object f m'' a

mkFileMenuSignal :: (Monad m, MonadIO m'')
                 => (FileMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p buffer m object m'' a
mkFileMenuSignal = mkGuiEnvSignalFor fileMenu

mkProjectMenuSignal :: (Monad m, MonadIO m'')
                 => (ProjectMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p buffer m object m'' a
mkProjectMenuSignal = mkGuiEnvSignalFor projectMenu

mkProjectViewerSignal :: (Monad m, MonadIO m'')
                 => (ProjectViewer -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p buffer m object m'' a
mkProjectViewerSignal = mkGuiEnvSignalFor projectViewer

mkProjectViewerSignal2 :: (Monad m, MonadIO m'', Functor f)
                 => (ProjectViewer -> object)
                 -> Signal object (f (m'' a))
                 -> MainWindowSignal2 proxy m' p buffer m object f m'' a
mkProjectViewerSignal2 = mkGuiEnvSignal2For projectViewer

newClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p buffer m MenuItem IO ()
newClickedEvent = newButton `mkFileMenuSignal` menuItemActivated


openClickedEvent :: (Monad m) 
                 => MainWindowSignal proxy m' p buffer m MenuItem IO ()
openClickedEvent = openButton `mkFileMenuSignal` menuItemActivated

digestClickedEvent :: (Monad m) 
                   => MainWindowSignal proxy m' p buffer m MenuItem IO ()
digestClickedEvent = digestButton `mkFileMenuSignal` menuItemActivated

saveClickedEvent :: (Monad m) 
                 => MainWindowSignal proxy m' p buffer m MenuItem IO ()
saveClickedEvent = saveButton `mkFileMenuSignal` menuItemActivated

saveProjectClickedEvent :: (Monad m) 
                        => MainWindowSignal proxy m' p buffer m MenuItem IO ()
saveProjectClickedEvent = saveProjectButton `mkFileMenuSignal` menuItemActivated

buildClickedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p buffer m MenuItem IO ()
buildClickedEvent = buildButton `mkProjectMenuSignal` menuItemActivated

runClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p buffer m MenuItem IO ()
runClickedEvent = runButton `mkProjectMenuSignal` menuItemActivated

declClickedEvent :: (Monad m) 
                 => MainWindowSignal2 proxy m' p buffer m TreeView 
                          (Compose ((->) TreePath) ((->) TreeViewColumn)) IO ()
declClickedEvent = projectView `mkProjectViewerSignal2` (editSignal rowActivated getCompose)

projectViewClickedEvent :: (Monad m) 
                     => MainWindowSignal proxy m' p buffer m TreeView (EventM EButton) Bool
projectViewClickedEvent = projectView `mkProjectViewerSignal` buttonPressEvent

windowClosedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p buffer m Window (EventM EAny) Bool
windowClosedEvent = window `mkGuiEnvSignal` deleteEvent

getProjectPathClicked :: (MonadIO m)
                      => Point
                      -> MainWindow 
                      -> m (Maybe (TreePath, TreeViewColumn, Point))
getProjectPathClicked p = liftIO . flip treeViewGetPathAtPos p . projectView . projectViewer 

{-addAccel :: (WidgetClass object, MonadIO m) 
         => (MainWindow -> object) 
         -> String 
         -> MainWindow 
         -> _ 
         -> String 
         -> [Modifier] 
         -> [AccelFlags] 
         -> m ()-}
addAccel f e w g kn ms fs = do
    k <- keyvalFromName $ pack kn
    widgetAddAccelerator (f w) e g k ms fs

{-addAccelGroup :: (MonadIO m) => MainWindow -> AccelGroup -> m ()-}
addAccelGroup w g = liftIO $ window w `windowAddAccelGroup` g

addFileMenuAccelerator f e = (f . fileMenu) `addAccel` e
addProjectMenuAccelerator f e = (f . projectMenu) `addAccel` e

{-addNewClickedEventAccelerator :: (MonadIO m)
                              => MainWindow 
                              -> _
                              -> String 
                              -> [Modifier] 
                              -> [AccelFlags] 
                              -> m ()-}
addNewClickedEventAccelerator = newButton `addFileMenuAccelerator` "activate"
addOpenClickedEventAccelerator = openButton `addFileMenuAccelerator` "activate"
addDigestClickedEventAccelerator = digestButton `addFileMenuAccelerator` "activate"
addSaveClickedEventAccelerator = saveButton `addFileMenuAccelerator` "activate"
addSaveProjectClickedEventAccelerator = saveProjectButton `addFileMenuAccelerator` "activate"

addBuildClickedEventAccelerator = buildButton `addProjectMenuAccelerator` "activate"
addRunClickedEventAccelerator = runButton `addProjectMenuAccelerator` "activate"


