module GuiLayout where

import Graphics.UI.Gtk

makeMainWindowWith f = do
    initGUI
    window <- windowNew
    f window
    widgetShowAll window
    putStrLn "Starting"
    mainGUI

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
    return projView

makeDeclView buffer container = do
    declView <- textViewNewWithBuffer buffer
    tableAttach
        container
        declView
        1 2
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
    return declView
    
makeOpenButton container = do
    openButton <- buttonNew
    buttonSetLabel openButton "Open"
    tableAttach
        container
        openButton
        0 1
        1 2
        [Expand,Fill] [Expand,Fill] 0 0
    return openButton

makeBuildButton container = do
    buildButton <- buttonNew
    buttonSetLabel buildButton "Build"
    tableAttach
        container
        buildButton
        1 2
        1 2
        [Expand,Fill] [Expand,Fill] 0 0
    return buildButton

makeBuildView buffer container = do
    buildView <- textViewNewWithBuffer buffer
    tableAttach
        container
        buildView
        0 2
        2 3
        [Expand,Fill] [Expand,Fill] 0 0
    return buildView

makeContainerWith f window = do
    container <- tableNew 2 3 False
    window `containerAdd` container
    f container
