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
    , saveSolutionClickedEvent
    , buildClickedEvent
    , runClickedEvent
    , windowClosedEvent
    , getSolutionPathClicked
    , addAccelGroup
    , addNewClickedEventAccelerator
    , addOpenClickedEventAccelerator
    , addDigestClickedEventAccelerator
    , addSaveClickedEventAccelerator
    , addSaveSolutionClickedEventAccelerator
    , addBuildClickedEventAccelerator
    , addRunClickedEventAccelerator
    ) where

import Data.Text

import Data.Functor.Compose

import Control.Monad.Trans

import Graphics.UI.Gtk

import BetterTextView

import GuiEnv
import GuiMonad

import SolutionTree

import GuiHelpers

data MainWindow
    = MainWindow
    { window :: Window
    , fileMenu :: FileMenu
    , projectMenu :: SolutionMenu
    , projectViewer :: SolutionViewer
    , buildViewer :: BuildViewer
    }  


--make :: GuiEnv proxy m p buffer -> (MainWindow -> m a) -> m ()

make :: (TextBufferClass buffer, MonadIO m)
     => (MainWindow -> GuiEnvT proxy m' p buffer m a) 
     -> GuiEnvT proxy m' p buffer m a
make f = makeMainWindowWith $ \window -> do
    renderer <- makeRenderer
    makeVBoxWith window $ \container -> do
        menuBar <- makeMainMenuBar container
        fileMenu <- makeFileMenu menuBar
        projectMenu <- makeSolutionMenu menuBar
        makeVPanedWith container $ \vbox -> do
            projectViewerBox <- makeSoloBox
            buildViewerBox <- makeSoloBox
            liftIO $ vbox `panedAdd1` projectViewerBox
            liftIO $ vbox `panedAdd2` buildViewerBox
            projectViewer <- makeSolutionViewer renderer 
                                               renderSolutionTreeElem
                                               projectViewerBox
            buildViewer <- makeBuildViewer buildViewerBox
            f MainWindow
              { window
              , fileMenu
              , projectMenu
              , projectViewer
              , buildViewer
              }

makeRenderer :: (MonadIO m) => GuiEnvT proxy m' p buffer m CellRendererText
makeRenderer = liftIO cellRendererTextNew

makeSoloBox :: (MonadIO m)
            => GuiEnvT proxy m' p buffer m VBox
makeSoloBox = liftIO $ vBoxNew False 0

makeVBoxWith :: (MonadIO m, ContainerClass self) 
             => self 
             -> (VBox -> GuiEnvT proxy m' p buffer m b) 
             -> GuiEnvT proxy m' p buffer m b
makeVBoxWith window f = do
    vbox <- liftIO $ vBoxNew False 0
    liftIO $ window `containerAdd` vbox
    f vbox

makeVPanedWith :: (MonadIO m, ContainerClass self) 
               => self 
               -> (VPaned -> GuiEnvT proxy m' p buffer m b) 
               -> GuiEnvT proxy m' p buffer m b
makeVPanedWith container f = do
    vbox <- liftIO $ vPanedNew
    liftIO $ container `containerAdd` vbox
    f vbox

makeHPanedWith :: (MonadIO m, ContainerClass self) 
               => self 
               -> (HPaned -> GuiEnvT proxy m' p buffer m b) 
               -> GuiEnvT proxy m' p buffer m b
makeHPanedWith container f = do
    hbox <- liftIO $ hPanedNew
    liftIO $ container `containerAdd` hbox
    f hbox

{-
makeContainerWith :: (MonadIO m, BoxClass self) => self -> (Table -> GuiEnvT proxy m' p buffer m b) -> GuiEnvT proxy m' p buffer m b
makeContainerWith vbox f = do
    container <- liftIO $ tableNew 2 3 False
    liftIO $ boxPackEnd vbox container PackGrow 0
    f container
-}

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
    , saveSolutionButton :: MenuItem
    }

makeFileMenu :: (MonadIO m) => MenuBar -> GuiEnvT proxy m' p buffer m FileMenu
makeFileMenu = makeFileMenuWith $ \fileMenu -> do
    newButton <- makeNewButton fileMenu
    openButton <- makeOpenButton fileMenu
    digestButton <- makeDigestButton fileMenu
    saveButton <- makeSaveButton fileMenu
    saveSolutionButton <- makeSaveSolutionButton fileMenu
    return FileMenu
           { newButton
           , openButton
           , digestButton
           , saveButton
           , saveSolutionButton
           }

makeFileMenuWith :: (MonadIO m) => (Menu -> GuiEnvT proxy m' p buffer m b) -> MenuBar -> GuiEnvT proxy m' p buffer m b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeNewButton = makeMenuButton "New Solution"

makeOpenButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveSolutionButton :: (MonadIO m) => Menu -> GuiEnvT proxy m' p buffer m MenuItem
makeSaveSolutionButton = makeMenuButton "Save Solution"

 
data SolutionMenu
    = SolutionMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    }

makeSolutionMenu :: (MonadIO m) => MenuBar -> GuiEnvT proxy m' p buffer m SolutionMenu
makeSolutionMenu = makeSolutionMenuWith $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return SolutionMenu
           { buildButton
           , runButton
           }

makeSolutionMenuWith :: (MonadIO m) => (Menu -> GuiEnvT proxy m' p buffer m b) -> MenuBar -> GuiEnvT proxy m' p buffer m b
makeSolutionMenuWith = makeMenuWith "Solution"

makeBuildButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT proxy m' p buffer m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT proxy m' p buffer m MenuItem
makeRunButton = makeMenuButton "Run"

data SolutionViewer
    = SolutionViewer
    { projectView :: TreeView
    , declView :: BetterTextView
    }

makeSolutionViewer :: (MonadIO m
                     , ContainerClass self
                     , CellRendererClass cell
                     , TextBufferClass buffer
                     ) 
                  => cell
                  -> (SolutionTreeElem -> [AttrOp cell])
                  -> self
                  -> GuiEnvT proxy m' p buffer m SolutionViewer
makeSolutionViewer renderer renderFunc container = do
    makeHPanedWith container $ \hbox -> do
        projectViewBox <- makeSoloBox
        declViewBox <- makeSoloBox
        liftIO $ hbox `panedAdd1` projectViewBox
        liftIO $ hbox `panedAdd2` declViewBox
        projectView <- makeProjView renderer projectViewBox renderFunc
        declView <- makeDeclView declViewBox
        return SolutionViewer
               { projectView
               , declView
               }

makeProjView :: ( MonadIO m
                , ContainerClass self
                , CellRendererClass cell
                , TextBufferClass buffer
                ) 
             => cell 
             -> self 
             -> (SolutionTreeElem -> [AttrOp cell]) 
             -> GuiEnvT proxy m' p buffer m TreeView
makeProjView renderer container renderFunc = do
    treeViewColumn <- liftIO treeViewColumnNew
    projView <- withGuiComponents $ flip withSolutionTree $ liftIO . treeViewNewWithModel
    _ <- liftIO $ treeViewAppendColumn projView treeViewColumn
    liftIO $ cellLayoutPackStart treeViewColumn renderer False
    withGuiComponents $ flip withSolutionTree $ \treeStore -> 
        liftIO $ cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
    scrollWindow <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ container `containerAdd` scrollWindow
        {-container
        scrollWindow
        0 1
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0-}
    liftIO $ scrollWindow `containerAdd` projView
    return projView

makeDeclView :: ( MonadIO m
                , ContainerClass self
                , TextBufferClass buffer
                ) 
             => self -> GuiEnvT proxy m' p buffer m BetterTextView
makeDeclView container = do
    declView <- withGuiComponents $ flip withEditorBuffer $ liftIO . betterTextViewNewWithBuffer
    monospace <- liftIO fontDescriptionNew
    liftIO $ monospace `fontDescriptionSetFamily` "monospace"
    liftIO $ declView `widgetModifyFont` Just monospace
    scrollWindow <- liftIO $ scrolledWindowNew Nothing Nothing
    {-
    liftIO $ tableAttach
        container
        scrollWindow
        1 2
        0 1
        [Expand,Fill] [Expand,Fill] 0 0
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    -}
    liftIO $ container `containerAdd` scrollWindow
    liftIO $ scrollWindow `containerAdd` declView
    return declView

data BuildViewer
    = BuildViewer
    { buildView :: TextView
    }

makeBuildViewer :: ( MonadIO m
                   , TextBufferClass buffer
                   , ContainerClass self
                   ) 
                => self -> GuiEnvT proxy m' p buffer m BuildViewer
makeBuildViewer container = do
    buildView <- makeBuildView container
    return BuildViewer
           { buildView
           }

makeBuildView :: ( MonadIO m
                 , TextBufferClass buffer
                 , ContainerClass self
                 ) 
              => self -> GuiEnvT proxy m' p buffer m TextView
makeBuildView container = do
    buildView <- withGuiComponents $ flip withBuildBuffer $ liftIO . textViewNewWithBuffer
    {-
    liftIO $ tableAttach
        container
        buildView
        0 2
        2 3
        [Expand,Fill] [Expand,Fill] 0 0
    -}
--        [Fill] [Fill] 0 0
--        [Expand] [Expand] 0 0
    scrollWindow <- liftIO $ scrolledWindowNew Nothing Nothing
    liftIO $ scrollWindow `containerAdd` buildView
    liftIO $ container `containerAdd` scrollWindow
    return buildView

makeMainWindowWith :: (MonadIO m) 
                   => (Window -> GuiEnvT proxy m' p buffer m a) 
                   -> GuiEnvT proxy m' p buffer m a
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

mkSolutionMenuSignal :: (Monad m, MonadIO m'')
                 => (SolutionMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p buffer m object m'' a
mkSolutionMenuSignal = mkGuiEnvSignalFor projectMenu

mkSolutionViewerSignal :: (Monad m, MonadIO m'')
                 => (SolutionViewer -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p buffer m object m'' a
mkSolutionViewerSignal = mkGuiEnvSignalFor projectViewer

mkSolutionViewerSignal2 :: (Monad m, MonadIO m'', Functor f)
                 => (SolutionViewer -> object)
                 -> Signal object (f (m'' a))
                 -> MainWindowSignal2 proxy m' p buffer m object f m'' a
mkSolutionViewerSignal2 = mkGuiEnvSignal2For projectViewer

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

saveSolutionClickedEvent :: (Monad m) 
                        => MainWindowSignal proxy m' p buffer m MenuItem IO ()
saveSolutionClickedEvent = saveSolutionButton `mkFileMenuSignal` menuItemActivated

buildClickedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p buffer m MenuItem IO ()
buildClickedEvent = buildButton `mkSolutionMenuSignal` menuItemActivated

runClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p buffer m MenuItem IO ()
runClickedEvent = runButton `mkSolutionMenuSignal` menuItemActivated

declClickedEvent :: (Monad m) 
                 => MainWindowSignal2 proxy m' p buffer m TreeView 
                          (Compose ((->) TreePath) ((->) TreeViewColumn)) IO ()
declClickedEvent = projectView `mkSolutionViewerSignal2` (editSignal rowActivated getCompose)

projectViewClickedEvent :: (Monad m) 
                     => MainWindowSignal proxy m' p buffer m TreeView (EventM EButton) Bool
projectViewClickedEvent = projectView `mkSolutionViewerSignal` buttonPressEvent

windowClosedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p buffer m Window (EventM EAny) Bool
windowClosedEvent = window `mkGuiEnvSignal` deleteEvent

getSolutionPathClicked :: (MonadIO m)
                      => Point
                      -> MainWindow 
                      -> m (Maybe (TreePath, TreeViewColumn, Point))
getSolutionPathClicked p = liftIO . flip treeViewGetPathAtPos p . projectView . projectViewer 

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
addSolutionMenuAccelerator f e = (f . projectMenu) `addAccel` e

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
addSaveSolutionClickedEventAccelerator = saveSolutionButton `addFileMenuAccelerator` "activate"

addBuildClickedEventAccelerator = buildButton `addSolutionMenuAccelerator` "activate"
addRunClickedEventAccelerator = runButton `addSolutionMenuAccelerator` "activate"


