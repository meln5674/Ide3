{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
module MainWindow
    ( MainWindow
    , make
    , newClickedEvent
    , openClickedEvent
    , digestClickedEvent
    , declClickedEvent
    , declViewClickedEvent
    , saveClickedEvent
    , saveProjectClickedEvent
    , buildClickedEvent
    , runClickedEvent
    , windowClosedEvent
    , getProjectPathClicked
    ) where

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


--make :: GuiEnv proxy m p buffer -> (MainWindow -> IO a) -> IO ()

make :: TextBufferClass buffer => GuiEnv proxy m p buffer -> (MainWindow -> IO a) -> IO a
make env f = withGuiComponents env $ \comp ->
    makeMainWindowWith $ \window -> 
        makeVBoxWith window $ \vbox -> 
            makeContainerWith vbox $ \container -> do
                renderer <- makeRenderer
                menuBar <- makeMainMenuBar vbox
                
                fileMenu <- makeFileMenu menuBar
                projectMenu <- makeProjectMenu menuBar
                projectViewer <- withProjectTree comp $ \treeStore -> 
                                 withEditorBuffer comp $ \buffer -> 
                                    makeProjectViewer treeStore 
                                                      buffer 
                                                      renderer 
                                                      renderProjectTreeElem
                                                      container
                buildViewer <- withBuildBuffer comp $ \buffer -> 
                                    makeBuildViewer buffer container
                f MainWindow
                  { window
                  , fileMenu
                  , projectMenu
                  , projectViewer
                  , buildViewer
                  }

makeRenderer :: IO CellRendererText
makeRenderer = cellRendererTextNew

makeVBoxWith :: ContainerClass self => self -> (VBox -> IO b) -> IO b
makeVBoxWith window f = do
    vbox <- vBoxNew False 0
    window `containerAdd` vbox
    f vbox

makeContainerWith :: BoxClass self => self -> (Table -> IO b) -> IO b
makeContainerWith vbox f = do
    container <- tableNew 2 3 False
    boxPackEnd vbox container PackGrow 0
    f container

makeMainMenuBar :: BoxClass self => self -> IO MenuBar
makeMainMenuBar vbox = do
    menuBar <- menuBarNew
    boxPackStart vbox menuBar PackNatural 0
    return menuBar

data FileMenu
    = FileMenu
    { newButton :: MenuItem
    , openButton :: MenuItem
    , digestButton :: MenuItem
    , saveButton :: MenuItem
    , saveProjectButton :: MenuItem
    }

makeFileMenu :: MenuBar -> IO FileMenu
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

makeFileMenuWith :: (Menu -> IO b) -> MenuBar -> IO b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: Menu -> IO MenuItem
makeNewButton = makeMenuButton "New Project"

makeOpenButton :: Menu -> IO MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: Menu -> IO MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: Menu -> IO MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveProjectButton :: Menu -> IO MenuItem
makeSaveProjectButton = makeMenuButton "Save Project"

 
data ProjectMenu
    = ProjectMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    }

makeProjectMenu :: MenuBar -> IO ProjectMenu
makeProjectMenu = makeProjectMenuWith $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return ProjectMenu
           { buildButton
           , runButton
           }

makeProjectMenuWith :: (Menu -> IO b) -> MenuBar -> IO b
makeProjectMenuWith = makeMenuWith "Project"

makeBuildButton :: MenuShellClass self => self -> IO MenuItem
makeBuildButton projectMenu = do
    buildButton <- menuItemNewWithLabel "Build"
    menuShellAppend projectMenu buildButton
    return buildButton

makeRunButton :: MenuShellClass self => self -> IO MenuItem
makeRunButton projectMenu = do
    runButton <- menuItemNewWithLabel "Run"
    menuShellAppend projectMenu runButton
    return runButton

data ProjectViewer
    = ProjectViewer
    { projectView :: TreeView
    , declView :: TextView
    }
makeProjectViewer :: ( TreeModelClass (model row)
                     , TextBufferClass buffer
                     , TableClass self
                     , CellRendererClass cell
                     , TypedTreeModelClass model
                     ) 
                  => model row
                  -> buffer
                  -> cell
                  -> (row -> [AttrOp cell])
                  -> self
                  -> IO ProjectViewer
makeProjectViewer treeStore buffer renderer renderFunc container = do
    projectView <- makeProjView treeStore renderer container renderFunc
    declView <- makeDeclView buffer container
    return ProjectViewer
           { projectView
           , declView
           }
makeProjView :: ( TreeModelClass (model row)
                , TableClass self
                , CellRendererClass cell
                , TypedTreeModelClass model
                ) 
             => model row 
             -> cell 
             -> self 
             -> (row -> [AttrOp cell]) 
             -> IO TreeView
makeProjView treeStore renderer container renderFunc = do
    treeViewColumn <- treeViewColumnNew
    projView <- treeViewNewWithModel treeStore
    _ <- treeViewAppendColumn projView treeViewColumn
    cellLayoutPackStart treeViewColumn renderer False
    cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
    scrollWindow <- scrolledWindowNew Nothing Nothing
    scrollWindow `containerAdd` projView
    tableAttach
        container
        scrollWindow
        0 1
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return projView

makeDeclView :: (TextBufferClass buffer, TableClass self) 
             => buffer -> self -> IO TextView
makeDeclView buffer container = do
    declView <- textViewNewWithBuffer buffer
    monospace <- fontDescriptionNew
    monospace `fontDescriptionSetFamily` "monospace"
    declView `widgetModifyFont` Just monospace
    scrollWindow <- scrolledWindowNew Nothing Nothing
    scrollWindow `containerAdd` declView
    tableAttach
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

makeBuildViewer :: (TextBufferClass buffer, TableClass self) 
                => buffer -> self -> IO BuildViewer
makeBuildViewer buffer container = do
    buildView <- makeBuildView buffer container
    return BuildViewer
           { buildView
           }

makeBuildView :: (TextBufferClass buffer, TableClass self) 
              => buffer -> self -> IO TextView
makeBuildView buffer container = do
    buildView <- textViewNewWithBuffer buffer
    tableAttach
        container
        buildView
        0 2
        2 3
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return buildView

makeMainWindowWith :: (Window -> IO a) -> IO a
makeMainWindowWith f = do
    _ <- initGUI
    window <- windowNew
    r <- f window
    widgetShowAll window
    putStrLn "Starting"
    mainGUI
    return r


type MainWindowSignal = GuiSignal MainWindow

mkFileMenuSignal :: (FileMenu -> object)
                 -> Signal object handler 
                 -> MainWindowSignal object handler
mkFileMenuSignal obj event = (obj . fileMenu) `mkGuiSignal` event

mkProjectMenuSignal :: (ProjectMenu -> object)
                    -> Signal object handler 
                    -> MainWindowSignal object handler
mkProjectMenuSignal obj event = (obj . projectMenu) `mkGuiSignal` event

mkProjectViewerSignal :: (ProjectViewer -> object)
                      -> Signal object handler 
                      -> MainWindowSignal object handler
mkProjectViewerSignal obj event = (obj . projectViewer) `mkGuiSignal` event

newClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
newClickedEvent = newButton `mkFileMenuSignal` buttonPressEvent

openClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
openClickedEvent = openButton `mkFileMenuSignal` buttonPressEvent

digestClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
digestClickedEvent = digestButton `mkFileMenuSignal` buttonPressEvent

saveClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
saveClickedEvent = saveButton `mkFileMenuSignal` buttonPressEvent

saveProjectClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
saveProjectClickedEvent = saveProjectButton `mkFileMenuSignal` buttonPressEvent

buildClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
buildClickedEvent = buildButton `mkProjectMenuSignal` buttonPressEvent

runClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
runClickedEvent = runButton `mkProjectMenuSignal` buttonPressEvent

declClickedEvent :: MainWindowSignal TreeView (TreePath -> TreeViewColumn -> IO ())
declClickedEvent = projectView `mkProjectViewerSignal` rowActivated

declViewClickedEvent :: MainWindowSignal TreeView (EventM EButton Bool)
declViewClickedEvent = projectView `mkProjectViewerSignal` buttonPressEvent

windowClosedEvent :: MainWindowSignal Window (EventM EAny Bool)
windowClosedEvent = window `mkGuiSignal` deleteEvent

getProjectPathClicked :: Point
                      -> MainWindow 
                      -> IO (Maybe (TreePath, TreeViewColumn, Point))
getProjectPathClicked p = flip treeViewGetPathAtPos p . projectView . projectViewer 
