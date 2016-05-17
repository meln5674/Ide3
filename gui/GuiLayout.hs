{-# LANGUAGE PolyKinds #-}
module GuiLayout 
    ( Gui
    , makeGui
    , withOpenButton
    , withBuildButton
    , withProjectView
    ) where

import Graphics.UI.Gtk

import GuiEnv
import GuiMonad

import ProjectTree

data Gui
    = Gui
    { openButton :: MenuItem
    , fileMenu :: Menu
    , menuBar :: MenuBar
    , buildButton :: Button
    , projectView :: TreeView
    , declView :: TextView
    , buildView :: TextView
    }

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
    tableAttach
        container
        projView
        0 1
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    return projView

makeDeclView buffer container = do
    declView <- textViewNewWithBuffer buffer
    tableAttach
        container
        declView
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



makeGui env f = withGuiComponents env $ \comp -> do
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
                openButton <- makeOpenButton fileMenu
                buildButton <- makeBuildButton container
                buildView <- withBuildBuffer comp $ \buffer ->
                    makeBuildView buffer container
                f $ Gui
                    openButton
                    fileMenu
                    menuBar
                    buildButton
                    projectView
                    declView
                    buildView

withOpenButton gui f = f $ openButton gui
withBuildButton gui f = f $ buildButton gui
withProjectView gui f = f $ projectView gui
