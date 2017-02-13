{-|
Module      : Dialogs.MainWindow
Description : Main application window
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Dialogs.MainWindow
    ( 
    -- * Window type
      MainWindow
    
    -- * Functions
    , make

    , SolutionPathCoords (..)
    , getSolutionPathClicked
    , FocusTarget (..)
    , setFocus
    , setSearchBarVisible
    , setSearchMode
    , scrollEditorCursorIntoView
    , setDeclViewEnabled
    , setBuildButtonEnabled
    
    , addAccelGroup

    -- * Signals
    , MainWindowSignal
    , newClickedEvent
    , openClickedEvent
    , digestClickedEvent
    , declClickedEvent
    , projectViewClickedEvent
    , saveClickedEvent
    , saveSolutionClickedEvent
    , buildClickedEvent
    , runClickedEvent
    , errorClickedEvent
    , windowClosedEvent
    , findClickedEvent
    , navigateClickedEvent
    --, searchClickedEvent
    , gotoDeclarationClickedEvent
    , backClickedEvent
    , forwardClickedEvent
    , declarationEditedEvent
    , newModuleClickedEvent
    , deleteProjectClickedEvent
    , newSubModuleClickedEvent
    , newPragmaClickedEvent
    , newExportClickedEvent
    , newImportClickedEvent
    , newDeclarationClickedEvent
    
    , solutionTreeQueryTooltipEvent
    
    -- * Accelerators
    , addNewClickedEventAccelerator
    , addOpenClickedEventAccelerator
    , addDigestClickedEventAccelerator
    , addSaveClickedEventAccelerator
    , addSaveSolutionClickedEventAccelerator
    , addBuildClickedEventAccelerator
    , addRunClickedEventAccelerator
    --, addFindClickedEventAccelerator
    --, addNavigateClickedEventAccelerator
    --, addSearchClickedEventAccelerator
    , addGotoDeclarationEventAccelerator
    , addBackEventAccelerator
    , addForwardEventAccelerator
    ) where

import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk hiding (TreePath, SearchBar)

import BetterTextView

import GuiClass.Types

import GuiEnv

import GuiHelpers

import qualified Dialogs.SearchBar as SearchBar

import SearchMode

import Dialogs.MainWindow.Types
import Dialogs.MainWindow.Menus
import Dialogs.MainWindow.Components
import Dialogs.MainWindow.Signals
import Dialogs.MainWindow.Accelerators


{-
Overlay
|
+-- Grid
|   |
|   +-- MenuBar
|   |
|   +-- VPaned
|       |
|       +-- HPaned
|       |   |
|       |   +-- TreeView 
|       |   |
|       |   +-- BetterTextView
|       |
|       +-- Notebook
|
+-- Grid
    |
    +-- SearchBar

-}

-- | Create the main application window
make :: (MonadIO m)
     => (MainWindow -> GuiEnvT {-proxy-} m' p  m a) 
     -> GuiEnvT {-proxy-} m' p  m a
make f = makeMainWindowWith $ \window -> do
    renderer <- makeRenderer
    makeOverlayWith window $ \overlay -> do
        searchBarBox <- vBoxNew False 0
        set overlay [ #expand := True ]
        --overlay `overlayAddOverlay` searchBarBox
        --overlay `containerAdd` searchBarBox
        --overlaySetOverlayPassThrough overlay searchBarBox True
        searchBar <- SearchBar.make searchBarBox
        SearchBar.setVisible searchBar False
        container <- new Grid [#orientation := OrientationVertical]
        set container [ #expand := True ]
        overlay `containerAdd` container
        menuBar <- new MenuBar []
        set menuBar [ #hexpand := True ]
        menus <- makeMenus menuBar
        gridAttach container menuBar 0 0 1 1
        --containerAdd container menuBar
        
        makeVPanedWith container $ \vbox -> do
            set vbox [ #vexpand := True ]
            solutionViewerBox <- makeSoloBox
            buildViewerBox <- makeSoloBox
            vbox `panedAdd1` solutionViewerBox
            vbox `panedAdd2` buildViewerBox
            solutionViewer <- makeSolutionViewer renderer 
                                               renderSolutionTreeElem
                                               solutionViewerBox
            buildViewer <- makeBuildViewer buildViewerBox
            f MainWindow
              { window
              , menus
              , solutionViewer
              , buildViewer
              , searchBar
              }

makeRenderer :: (MonadIO m) => GuiEnvT {-proxy-} m' p  m CellRendererText
makeRenderer = cellRendererTextNew

{-
makeContainerWith :: (MonadIO m, IsBox self) => self -> (Table -> GuiEnvT {-proxy-} m' p  m b) -> GuiEnvT {-proxy-} m' p  m b
makeContainerWith vbox f = do
    container <- tableNew 2 3 False
    boxPackEnd vbox container True True 0
    f container
-}

{-
makeMainMenuBar :: (MonadIO m, IsBox self) => self -> GuiEnvT {-proxy-} m' p  m MenuBar
makeMainMenuBar vbox = do
    menuBar <- menuBarNew
    boxPackStart vbox menuBar False False 0
    return menuBar
-}

makeMainWindowWith :: (MonadIO m) 
                   => (Window -> GuiEnvT {-proxy-} m' p  m a) 
                   -> GuiEnvT {-proxy-} m' p  m a
makeMainWindowWith f = do
    window <- new Window []
    r <- f window
    liftIO $ widgetShowAll window
    return r

getSolutionPathClicked :: (MonadIO m)
                      => SolutionPathCoords
                      -> MainWindow 
                      -> m (Maybe (TreePath, TreeViewColumn, (Int,Int)))
getSolutionPathClicked coords window = do
    (x, y) <- case coords of
        BinWindowCoords x' y' -> return (fromIntegral x', fromIntegral y')
        WidgetCoords x' y' -> treeViewConvertWidgetToBinWindowCoords (projectView $ solutionViewer window) (fromIntegral x') (fromIntegral y')
    result <- treeViewGetPathAtPos (projectView $ solutionViewer window) x y 
    case result of
        (True, Just path'', Just column, xoffset', yoffset') -> do
            path' <- treePathGetIndices path''
            let path = map fromIntegral path'
            --liftIO $ print path
            --treePathFree path''
            return $ Just (path, column, (xoffset, yoffset))
          where
            xoffset = fromIntegral xoffset'
            yoffset = fromIntegral yoffset'
        _ -> return Nothing

setSearchBarVisible :: (MonadIO m)
                    => MainWindow
                    -> Bool
                    -> m ()
setSearchBarVisible window v = SearchBar.setVisible (searchBar window) v

setSearchMode :: (MonadIO m)
              => MainWindow
              -> SearchMode
              -> m ()
setSearchMode window v = SearchBar.setSearchMode (searchBar window) v

setFocus :: (MonadIO m) => FocusTarget -> MainWindow -> m ()
setFocus FocusEditor = widgetGrabFocus . declView . solutionViewer
setFocus FocusLog = widgetGrabFocus . buildView . buildViewer
setFocus FocusErrorList = widgetGrabFocus . errorView . buildViewer
setFocus FocusSolutionTree = widgetGrabFocus . projectView . solutionViewer

scrollEditorCursorIntoView :: (MonadIO m) => MainWindow -> m ()
scrollEditorCursorIntoView window = do
    let textView = declView $ solutionViewer window
    buffer <- textViewGetBuffer textView
    startMark <- textBufferGetInsert buffer
    start <- textBufferGetIterAtMark buffer startMark
    line <- textIterGetLine start
    lineOffset <- textIterGetLineOffset start
    liftIO $ print (line, lineOffset)
    textViewScrollToMark textView startMark 0 True 0.5 0.5

setDeclViewEnabled :: (MonadIO m) => MainWindow -> Bool -> m ()
setDeclViewEnabled window enabled = setSub window [mkSubAttrOp (getWidget . declView . solutionViewer) (#editable := enabled)]

setBuildButtonEnabled :: ( MonadIO m ) => MainWindow -> Bool -> m ()
setBuildButtonEnabled window enabled = set (buildButton $ solutionMenu $ menus window) [#sensitive := enabled]


