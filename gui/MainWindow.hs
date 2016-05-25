{-# LANGUAGE PolyKinds #-}
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
make env f = withGuiComponents env $ \comp -> do
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
                f $ MainWindow
                         window
                         fileMenu
                         projectMenu
                         projectViewer
                         buildViewer

makeRenderer = cellRendererTextNew

makeVBoxWith window f = do
    vbox <- vBoxNew False 0
    window `containerAdd` vbox
    f vbox

makeContainerWith vbox f = do
    container <- tableNew 2 3 False
    boxPackEnd vbox container PackGrow 0
    f container


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

makeFileMenu = makeFileMenuWith $ \fileMenu -> do
    newButton <- makeNewButton fileMenu
    openButton <- makeOpenButton fileMenu
    digestButton <- makeDigestButton fileMenu
    saveButton <- makeSaveButton fileMenu
    saveProjectButton <- makeSaveProjectButton fileMenu
    return $ FileMenu
             newButton
             openButton
             digestButton
             saveButton
             saveProjectButton

makeFileMenuWith = makeMenuWith "File"

{-
makeFileMenuWith menuBar f = do
    fileMenuItem <- menuItemNewWithLabel "File"
    fileMenu <- menuNew
    menuShellAppend menuBar fileMenuItem
    menuItemSetSubmenu fileMenuItem fileMenu
    f fileMenu
-}

makeNewButton = makeMenuButton "New Project"
makeOpenButton = makeMenuButton "Open"
makeDigestButton = makeMenuButton "Digest"
makeSaveButton = makeMenuButton "Save"
makeSaveProjectButton = makeMenuButton "Save Project"

 
data ProjectMenu
    = ProjectMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    }

makeProjectMenu = makeProjectMenuWith $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return $ ProjectMenu
             buildButton
             runButton

{-
makeProjectMenuWith menuBar f = do
    projectMenuItem <- menuItemNewWithLabel "Project"
    projectMenu <- menuNew
    menuShellAppend menuBar projectMenuItem
    menuItemSetSubmenu projectMenuItem projectMenu
    f projectMenu
-}

makeProjectMenuWith = makeMenuWith "Project"

makeBuildButton projectMenu = do
    buildButton <- menuItemNewWithLabel "Build"
    menuShellAppend projectMenu buildButton
    return buildButton

makeRunButton projectMenu = do
    runButton <- menuItemNewWithLabel "Run"
    menuShellAppend projectMenu runButton
    return runButton

data ProjectViewer
    = ProjectViewer
    { projectView :: TreeView
    , declView :: TextView
    }

makeProjectViewer treeStore buffer renderer renderFunc container = do
    projectView <- makeProjView treeStore renderer container renderFunc
    declView <- makeDeclView buffer container
    return $ ProjectViewer
             projectView
             declView

makeProjView treeStore renderer container renderFunc = do
    treeViewColumn <- treeViewColumnNew
    projView <- treeViewNewWithModel treeStore
    treeViewAppendColumn projView treeViewColumn
    cellLayoutPackStart treeViewColumn renderer False
    cellLayoutSetAttributes treeViewColumn renderer treeStore $ renderFunc
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

makeBuildViewer buffer container = do
    buildView <- makeBuildView buffer container
    return $ BuildViewer
             buildView

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

{-
    { window :: Window
    , openButton :: MenuItem
    , fileMenu :: Menu
    , menuBar :: MenuBar
    , buildButton :: Button
    , projectView :: TreeView
    , declView :: TextView
    , buildView :: TextView
    , saveButton :: MenuItem
    , saveProjectButton :: MenuItem
    , newButton :: MenuItem
    }
-}

makeMainWindowWith :: (Window -> IO a) -> IO ()
makeMainWindowWith f = do
    initGUI
    window <- windowNew
    f window
    widgetShowAll window
    putStrLn "Starting"
    mainGUI









type MainWindowSignal = GuiSignal MainWindow


mkFileMenuSignal obj event = (obj . fileMenu) `mkGuiSignal` event
mkProjectViewerSignal obj event = (obj . projectViewer) `mkGuiSignal` event
--mkProjectViewerSignalWith obj event handler = (obj . projectViewer) `mkGuiSignalWith` event $ handler
mkProjectMenuSignal obj event = (obj . projectMenu) `mkGuiSignal` event

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

--getDeclClicked :: MainWindow -> Point -> (MAybe
getProjectPathClicked p = flip treeViewGetPathAtPos p . projectView . projectViewer 
