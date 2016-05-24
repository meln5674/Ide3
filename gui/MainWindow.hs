{-# LANGUAGE PolyKinds #-}
module MainWindow
    ( MainWindow
    , make
    , newClickedEvent
    , openClickedEvent
    , declClickedEvent
    , buildClickedEvent
    , saveClickedEvent
    , saveProjectClickedEvent
    ) where

import Graphics.UI.Gtk

import GuiEnv
import GuiMonad

import ProjectTree

import Signal

data MainWindow
    = MainWindow
    { openButton :: MenuItem
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


makeMainWindowWith :: (Window -> IO a) -> IO ()
makeMainWindowWith f = do
    initGUI
    window <- windowNew
    f window
    widgetShowAll window
    putStrLn "Starting"
    mainGUI


makeMainMenuBar vbox = do
    menuBar <- menuBarNew
    boxPackStart vbox menuBar PackNatural 0
    return menuBar

makeRenderer = cellRendererTextNew

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

makeFileMenu menuBar = do
    fileMenuItem <- menuItemNewWithLabel "File"
    fileMenu <- menuNew
    menuShellAppend menuBar fileMenuItem
    menuItemSetSubmenu fileMenuItem fileMenu
    return fileMenu

{-    
makeOpenButton container = do
    openButton <- buttonNew
    buttonSetLabel openButton "Open"
    tableAttach
        container
        openButton
        0 1
        1 2
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return openButton
-}

makeOpenButton fileMenu = do
    openButton <- menuItemNewWithLabel "Open"
    menuShellAppend fileMenu openButton
    return openButton

makeSaveButton fileMenu = do
    saveButton <- menuItemNewWithLabel "Save"
    menuShellAppend fileMenu saveButton
    return saveButton

makeSaveProjectButton fileMenu = do
    saveProjectButton <- menuItemNewWithLabel "Save Project"
    menuShellAppend fileMenu saveProjectButton
    return saveProjectButton

makeNewButton fileMenu = do
    newButton <- menuItemNewWithLabel "New Project"
    menuShellAppend fileMenu newButton
    return newButton

makeBuildButton container = do
    buildButton <- buttonNew
    buttonSetLabel buildButton "Build"
    tableAttach
        container
        buildButton
        0 2
        1 2
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return buildButton

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

makeVBoxWith window f = do
    vbox <- vBoxNew False 0
    window `containerAdd` vbox
    f vbox

makeContainerWith vbox f = do
    container <- tableNew 2 3 False
    boxPackEnd vbox container PackGrow 0
    f container

--make :: GuiEnv proxy m p buffer -> (MainWindow -> IO a) -> IO ()
make env f = withGuiComponents env $ \comp -> do
    makeMainWindowWith $ \window -> 
        makeVBoxWith window $ \vbox -> 
            makeContainerWith vbox $ \container -> do
                renderer <- makeRenderer
                projectView <- withProjectTree comp $ \treeStore ->
                    makeProjView treeStore renderer container renderProjectTreeElem
                declView <- withEditorBuffer comp $ \buffer ->
                    makeDeclView buffer container 
                
                menuBar <- makeMainMenuBar vbox
                fileMenu <- makeFileMenu menuBar
                newButton <- makeNewButton fileMenu
                openButton <- makeOpenButton fileMenu
                saveButton <- makeSaveButton fileMenu
                saveProjectButton <- makeSaveProjectButton fileMenu
                buildButton <- makeBuildButton container
                buildView <- withBuildBuffer comp $ flip makeBuildView container
                f $ MainWindow
                    openButton
                    fileMenu
                    menuBar
                    buildButton
                    projectView
                    declView
                    buildView
                    saveButton
                    saveProjectButton
                    newButton

type MainWindowSignal = GuiSignal MainWindow


newClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
newClickedEvent = newButton `mkGuiSignal` buttonPressEvent

openClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
openClickedEvent = openButton `mkGuiSignal` buttonPressEvent

declClickedEvent :: MainWindowSignal TreeView (TreePath -> TreeViewColumn -> IO ())
declClickedEvent = projectView `mkGuiSignal` rowActivated

buildClickedEvent :: MainWindowSignal Button (EventM EButton Bool)
buildClickedEvent = buildButton `mkGuiSignal` buttonPressEvent

saveClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
saveClickedEvent = saveButton `mkGuiSignal` buttonPressEvent

saveProjectClickedEvent :: MainWindowSignal MenuItem (EventM EButton Bool)
saveProjectClickedEvent = saveProjectButton `mkGuiSignal` buttonPressEvent

