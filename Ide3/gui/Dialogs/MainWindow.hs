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
    --, findClickedEvent
    --, navigateClickedEvent
    --, searchClickedEvent
    , gotoDeclarationClickedEvent
    , backClickedEvent
    , forwardClickedEvent
    , getSolutionPathClicked
    --, setSearchBarVisible
    --, setSearchMode
    , addAccelGroup
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

import Data.Text

import Data.Functor.Compose

import Control.Monad.Trans

import Graphics.UI.Gtk

import Ide3.Types

import BetterTextView

import ErrorParser.Types

import GuiEnv
import GuiMonad

import SolutionTree

import GuiHelpers

import Dialogs.SearchBar (SearchBar)
import qualified Dialogs.SearchBar as SearchBar

import SearchMode
import DeclarationPath

import GuiClass (SolutionTreeElem (..))

data MainWindow
    = MainWindow
    { window :: Window
    , fileMenu :: FileMenu
    , projectMenu :: SolutionMenu
    , searchMenu :: SearchMenu
    , navigationMenu :: NavigationMenu
    , projectViewer :: SolutionViewer
    , buildViewer :: BuildViewer
    --, searchBar :: SearchBar
    }  


renderSolutionTreeElem :: CellRendererTextClass o => SolutionTreeElem -> [AttrOp o]
renderSolutionTreeElem (ProjectElem (ProjectInfo n)) = [cellText := n]
renderSolutionTreeElem (ModuleElem (ModuleInfo (Symbol s)) _) = [cellText := s]
renderSolutionTreeElem (ModuleElem (UnamedModule (Just path)) _) = [cellText := path]
renderSolutionTreeElem (ModuleElem (UnamedModule Nothing) _) = [cellText := "???"]
renderSolutionTreeElem (DeclElem s) = [cellText := getSymbol $ getDeclarationInfo s]
renderSolutionTreeElem ImportsElem = [cellText := "Imports"]
renderSolutionTreeElem ExportsElem = [cellText := "Exports"]
renderSolutionTreeElem PragmasElem = [cellText := "Pragmas"]
renderSolutionTreeElem (ImportElem _ (WithBody _ importBody)) = [cellText := importBody] 
renderSolutionTreeElem (ExportElem _ (WithBody _ exportBody)) = [cellText := exportBody] 
renderSolutionTreeElem (PragmaElem p) = [cellText := p]

renderImageCell :: CellRendererPixbufClass o => Error ItemPath -> [AttrOp o]
renderImageCell (Warning _ _ _ _) = [cellPixbufStockId := stockDialogWarning]
renderImageCell (Error _ _ _ _) = [cellPixbufStockId := stockDialogError]

renderProjectCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderProjectCell e = [cellText := unProjectInfo pji]
  where
    (ProjectChild pji _) = errorLocation e

renderModuleCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderModuleCell e = [cellText := moduleInfoString mi "???"]
  where
    (ProjectChild _ (ModuleChild mi _)) = errorLocation e

renderDeclarationCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderDeclarationCell e = [cellText := text]
  where
    (ProjectChild _ (ModuleChild _ x)) = errorLocation e
    text = case x of
        Just x -> case x of
            HeaderCommentString _ -> "[MODULE HEADER]"
            PragmaString p -> "[PRAGMA] " ++ p
            ImportString i -> body i
            ExportString e -> "[EXPORT] " ++ body e
            DeclarationString di -> getSymbol $ getDeclarationInfo $ item di
        Nothing -> ""

renderRowCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderRowCell (Warning _ row _ _) = [cellText := show row]
renderRowCell (Error _ row _ _) = [cellText := show row]

renderColumnCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderColumnCell (Warning _ _ col _) = [cellText := show col]
renderColumnCell (Error _ _ col _) = [cellText := show col]

renderMessageCell :: CellRendererTextClass o => Error ItemPath -> [AttrOp o]
renderMessageCell (Warning _ _ _ msg) = [cellText := msg]
renderMessageCell (Error _ _ _ msg) = [cellText := msg]




--make :: GuiEnv {-proxy-} m p buffer -> (MainWindow -> m a) -> m ()

make :: (MonadIO m)
     => (MainWindow -> GuiEnvT {-proxy-} m' p  m a) 
     -> GuiEnvT {-proxy-} m' p  m a
make f = makeMainWindowWith $ \window -> do
    renderer <- makeRenderer
    --makeOverlayWith window $ \overlay -> do
        --searchBarBox <- liftIO $ vBoxNew False 0
        --liftIO $ overlay `overlayAdd` searchBarBox
        --searchBar <- SearchBar.make searchBarBox
        --liftIO $ SearchBar.setVisible searchBar False
        --makeVBoxWith overlay $ \container -> do
    makeVBoxWith window $ \container -> do
        menuBar <- makeMainMenuBar container
        fileMenu <- makeFileMenu menuBar
        projectMenu <- makeSolutionMenu menuBar
        searchMenu <- makeSearchMenu menuBar
        navigationMenu <- makeNavigationMenu menuBar
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
              , searchMenu
              , projectViewer
              , navigationMenu
              , buildViewer
              --, searchBar
              }

makeRenderer :: (MonadIO m) => GuiEnvT {-proxy-} m' p  m CellRendererText
makeRenderer = liftIO cellRendererTextNew

makeSoloBox :: (MonadIO m)
            => GuiEnvT {-proxy-} m' p  m VBox
makeSoloBox = liftIO $ vBoxNew False 0


makeVPanedWith :: (MonadIO m, ContainerClass self) 
               => self 
               -> (VPaned -> GuiEnvT {-proxy-} m' p  m b) 
               -> GuiEnvT {-proxy-} m' p  m b
makeVPanedWith container f = do
    vbox <- liftIO $ vPanedNew
    liftIO $ container `containerAdd` vbox
    f vbox

makeHPanedWith :: (MonadIO m, ContainerClass self) 
               => self 
               -> (HPaned -> GuiEnvT {-proxy-} m' p  m b) 
               -> GuiEnvT {-proxy-} m' p  m b
makeHPanedWith container f = do
    hbox <- liftIO $ hPanedNew
    liftIO $ container `containerAdd` hbox
    f hbox

{-
makeContainerWith :: (MonadIO m, BoxClass self) => self -> (Table -> GuiEnvT {-proxy-} m' p  m b) -> GuiEnvT {-proxy-} m' p  m b
makeContainerWith vbox f = do
    container <- liftIO $ tableNew 2 3 False
    liftIO $ boxPackEnd vbox container PackGrow 0
    f container
-}

makeMainMenuBar :: (MonadIO m, BoxClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuBar
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

makeFileMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m FileMenu
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

makeFileMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeFileMenuWith = makeMenuWith "File"

makeNewButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeNewButton = makeMenuButton "New Solution"

makeOpenButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeOpenButton = makeMenuButton "Open"

makeDigestButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeDigestButton = makeMenuButton "Digest"

makeSaveButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeSaveButton = makeMenuButton "Save"

makeSaveSolutionButton :: (MonadIO m) => Menu -> GuiEnvT {-proxy-} m' p  m MenuItem
makeSaveSolutionButton = makeMenuButton "Save Solution"

 
data SolutionMenu
    = SolutionMenu
    { buildButton :: MenuItem
    , runButton :: MenuItem
    }

makeSolutionMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m SolutionMenu
makeSolutionMenu = makeSolutionMenuWith $ \projectMenu -> do
    buildButton <- makeBuildButton projectMenu
    runButton <- makeRunButton projectMenu
    return SolutionMenu
           { buildButton
           , runButton
           }

makeSolutionMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeSolutionMenuWith = makeMenuWith "Solution"

makeBuildButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeRunButton = makeMenuButton "Run"

data SearchMenu
    = SearchMenu
    { findButton :: MenuItem
    , navigateButton :: MenuItem
    , gotoDeclarationButton :: MenuItem
    }

makeSearchMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p  m SearchMenu
makeSearchMenu = makeSearchMenuWith $ \searchMenu -> do
    findButton <- makeFindButton searchMenu
    navigateButton <- makeNavigateButton searchMenu
    gotoDeclarationButton <- makeGotoDeclarationButton searchMenu
    return SearchMenu
        { findButton
        , navigateButton
        , gotoDeclarationButton
        }

makeSearchMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeSearchMenuWith = makeMenuWith "Search"

makeFindButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeFindButton = makeMenuButton "Find"

makeNavigateButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeNavigateButton = makeMenuButton "Navigate"

makeGotoDeclarationButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeGotoDeclarationButton = makeMenuButton "Go to Declaration"

data NavigationMenu
    = NavigationMenu
    { backButton :: MenuItem
    , forwardButton :: MenuItem
    }

makeNavigationMenu :: (MonadIO m) => MenuBar -> GuiEnvT {-proxy-} m' p m NavigationMenu
makeNavigationMenu = makeNavigationMenuWith $ \navigationMenu -> do
    backButton <- makeBackButton navigationMenu
    forwardButton <- makeForwardButton navigationMenu
    return NavigationMenu
        { backButton
        , forwardButton
        }

makeNavigationMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p m b) -> MenuBar -> GuiEnvT {-proxy-} m' p m b
makeNavigationMenuWith = makeMenuWith "Navigation"

makeBackButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBackButton = makeMenuButton "Back"

makeForwardButton :: (MonadIO m, MenuShellClass self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeForwardButton = makeMenuButton "Forward"

data SolutionViewer
    = SolutionViewer
    { projectView :: TreeView
    , declView :: BetterTextView
    }

makeSolutionViewer :: (MonadIO m
                     , ContainerClass self
                     , CellRendererClass cell
                     ) 
                  => cell
                  -> (SolutionTreeElem -> [AttrOp cell])
                  -> self
                  -> GuiEnvT {-proxy-} m' p  m SolutionViewer
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
                ) 
             => cell 
             -> self 
             -> (SolutionTreeElem -> [AttrOp cell]) 
             -> GuiEnvT {-proxy-} m' p  m TreeView
makeProjView renderer container renderFunc = makeScrolledWindowWith container $ \scrollWindow -> do
    treeViewColumn <- liftIO treeViewColumnNew
    projView <- withGuiComponents $ flip withSolutionTree $ liftIO . treeViewNewWithModel
    _ <- liftIO $ treeViewAppendColumn projView treeViewColumn
    liftIO $ cellLayoutPackStart treeViewColumn renderer False
    withGuiComponents $ flip withSolutionTree $ \treeStore -> 
        liftIO $ cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
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
                ) 
             => self -> GuiEnvT {-proxy-} m' p  m BetterTextView
makeDeclView container = makeScrolledWindowWith container $ \scrollWindow -> do
    declView <- withGuiComponents $ flip withEditorBuffer $ liftIO . betterTextViewNewWithBuffer
    monospace <- liftIO fontDescriptionNew
    liftIO $ monospace `fontDescriptionSetFamily` "monospace"
    liftIO $ declView `widgetModifyFont` Just monospace
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
    liftIO $ scrollWindow `containerAdd` declView
    return declView

data BuildViewer
    = BuildViewer
    { buildView :: TextView
    , errorView :: TreeView
    }

makeBuildViewer :: ( MonadIO m
                   , ContainerClass self
                   ) 
                => self -> GuiEnvT {-proxy-} m' p  m BuildViewer
makeBuildViewer container = do
    makeNotebookWith container $ \notebook -> do
        buildView <- makeNotebookPageWith notebook "Log" makeBuildView
        errorView <- makeNotebookPageWith notebook "Errors" makeErrorView
       
            
        return BuildViewer
             { buildView
             , errorView
             }

makeBuildView :: ( MonadIO m
                 , ContainerClass self
                 ) 
              => self -> GuiEnvT {-proxy-} m' p  m TextView
makeBuildView container = makeScrolledWindowWith container $ \scrollWindow -> do
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
    liftIO $ scrollWindow `containerAdd` buildView
    return buildView


makeErrorView :: ( MonadIO m
                 , ContainerClass self
                 )
              => self -> GuiEnvT {-proxy-} m' p m TreeView
makeErrorView container = do
    withGuiComponents 
        $ flip withErrorList 
        $ \list -> makeScrolledWindowWith container 
        $ \scrollWindow -> liftIO $ do
        
            errorView <- treeViewNewWithModel list
            
            imageColumn <- treeViewColumnNew
            projectColumn <- treeViewColumnNew
            moduleColumn <- treeViewColumnNew
            declarationColumn <- treeViewColumnNew
            rowColumn <- treeViewColumnNew
            columnColumn <- treeViewColumnNew
            messageColumn <- treeViewColumnNew
            renderer <- cellRendererTextNew
            imageRenderer <- cellRendererPixbufNew
            
            treeViewColumnSetTitle projectColumn "Project"
            treeViewColumnSetTitle moduleColumn "Module"
            treeViewColumnSetTitle declarationColumn "Location"
            treeViewColumnSetTitle rowColumn "Row"
            treeViewColumnSetTitle columnColumn "Column"
            treeViewColumnSetTitle messageColumn "Message"
            
            treeViewColumnPackStart imageColumn imageRenderer True
            treeViewColumnPackStart projectColumn renderer True
            treeViewColumnPackStart moduleColumn renderer True
            treeViewColumnPackStart declarationColumn renderer True
            treeViewColumnPackStart rowColumn renderer True
            treeViewColumnPackStart columnColumn renderer True
            treeViewColumnPackStart messageColumn renderer True
            
            cellLayoutSetAttributes imageColumn imageRenderer list renderImageCell
            cellLayoutSetAttributes projectColumn renderer list renderProjectCell
            cellLayoutSetAttributes moduleColumn renderer list renderModuleCell
            cellLayoutSetAttributes declarationColumn renderer list renderDeclarationCell
            cellLayoutSetAttributes rowColumn renderer list renderRowCell
            cellLayoutSetAttributes columnColumn renderer list renderColumnCell
            cellLayoutSetAttributes messageColumn renderer list renderMessageCell
            
            {-
            treeViewColumnSetExpand declarationColumn True
            -}
            treeViewColumnSetExpand messageColumn True
            
            
            treeViewAppendColumn errorView imageColumn
            treeViewAppendColumn errorView projectColumn
            treeViewAppendColumn errorView moduleColumn
            treeViewAppendColumn errorView declarationColumn
            treeViewAppendColumn errorView rowColumn
            treeViewAppendColumn errorView columnColumn
            treeViewAppendColumn errorView messageColumn
            
            set projectColumn [treeViewColumnResizable := True]
            set moduleColumn [treeViewColumnResizable := True]
            set declarationColumn [treeViewColumnResizable := True]
            set messageColumn [treeViewColumnResizable := True]
            
            scrollWindow `containerAdd` errorView
            
            return errorView

makeMainWindowWith :: (MonadIO m) 
                   => (Window -> GuiEnvT {-proxy-} m' p  m a) 
                   -> GuiEnvT {-proxy-} m' p  m a
makeMainWindowWith f = do
    window <- liftIO $ do
        windowNew
    r <- f window
    liftIO $ widgetShowAll window
    return r

type MainWindowSignal proxy m' p  m object m'' a
    = GuiEnvSignal proxy m' p  m MainWindow object m'' a

type MainWindowSignal2 proxy m' p  m object f m'' a
    = GuiEnvSignal2 proxy m' p  m MainWindow object f m'' a

mkFileMenuSignal :: (Monad m, MonadIO m'')
                 => (FileMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p  m object m'' a
mkFileMenuSignal = mkGuiEnvSignalFor fileMenu

mkSolutionMenuSignal :: (Monad m, MonadIO m'')
                 => (SolutionMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p  m object m'' a
mkSolutionMenuSignal = mkGuiEnvSignalFor projectMenu

mkSearchMenuSignal :: (Monad m, MonadIO m'')
                 => (SearchMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p  m object m'' a
mkSearchMenuSignal = mkGuiEnvSignalFor searchMenu

mkNavigationMenuSignal :: (Monad m, MonadIO m'')
                 => (NavigationMenu -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p  m object m'' a
mkNavigationMenuSignal = mkGuiEnvSignalFor navigationMenu

mkSolutionViewerSignal :: (Monad m, MonadIO m'')
                 => (SolutionViewer -> object)
                 -> Signal object (m'' a)
                 -> MainWindowSignal proxy m' p  m object m'' a
mkSolutionViewerSignal = mkGuiEnvSignalFor projectViewer

mkSolutionViewerSignal2 :: (Monad m, MonadIO m'', Functor f)
                 => (SolutionViewer -> object)
                 -> Signal object (f (m'' a))
                 -> MainWindowSignal2 proxy m' p  m object f m'' a
mkSolutionViewerSignal2 = mkGuiEnvSignal2For projectViewer

newClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
newClickedEvent = newButton `mkFileMenuSignal` menuItemActivated


openClickedEvent :: (Monad m) 
                 => MainWindowSignal proxy m' p  m MenuItem IO ()
openClickedEvent = openButton `mkFileMenuSignal` menuItemActivated

digestClickedEvent :: (Monad m) 
                   => MainWindowSignal proxy m' p  m MenuItem IO ()
digestClickedEvent = digestButton `mkFileMenuSignal` menuItemActivated

saveClickedEvent :: (Monad m) 
                 => MainWindowSignal proxy m' p  m MenuItem IO ()
saveClickedEvent = saveButton `mkFileMenuSignal` menuItemActivated

saveSolutionClickedEvent :: (Monad m) 
                        => MainWindowSignal proxy m' p  m MenuItem IO ()
saveSolutionClickedEvent = saveSolutionButton `mkFileMenuSignal` menuItemActivated

buildClickedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p  m MenuItem IO ()
buildClickedEvent = buildButton `mkSolutionMenuSignal` menuItemActivated

runClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
runClickedEvent = runButton `mkSolutionMenuSignal` menuItemActivated

declClickedEvent :: (Monad m) 
                 => MainWindowSignal2 proxy m' p  m TreeView 
                          (Compose ((->) TreePath) ((->) TreeViewColumn)) IO ()
declClickedEvent = projectView `mkSolutionViewerSignal2` (editSignal rowActivated getCompose)

projectViewClickedEvent :: (Monad m) 
                     => MainWindowSignal proxy m' p  m TreeView (EventM EButton) Bool
projectViewClickedEvent = projectView `mkSolutionViewerSignal` buttonPressEvent

windowClosedEvent :: (Monad m) 
                  => MainWindowSignal proxy m' p  m Window (EventM EAny) Bool
windowClosedEvent = window `mkGuiEnvSignal` deleteEvent

findClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
findClickedEvent = findButton `mkSearchMenuSignal` menuItemActivated

navigateClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
navigateClickedEvent = navigateButton `mkSearchMenuSignal` menuItemActivated

gotoDeclarationClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
gotoDeclarationClickedEvent = gotoDeclarationButton `mkSearchMenuSignal` menuItemActivated

backClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
backClickedEvent = backButton `mkNavigationMenuSignal` menuItemActivated

forwardClickedEvent :: (Monad m) 
                => MainWindowSignal proxy m' p  m MenuItem IO ()
forwardClickedEvent = forwardButton `mkNavigationMenuSignal` menuItemActivated



{-
searchClickedEvent :: (Monad m)
                   => MainWindowSignal proxy m' p  m Button IO ()
searchClickedEvent = wrapGuiEnvSignal searchBar SearchBar.searchClickedEvent
-}



getSolutionPathClicked :: (MonadIO m)
                      => Point
                      -> MainWindow 
                      -> m (Maybe (TreePath, TreeViewColumn, Point))
getSolutionPathClicked p = liftIO . flip treeViewGetPathAtPos p . projectView . projectViewer 

{-
setSearchBarVisible :: (MonadIO m)
                    => MainWindow
                    -> Bool
                    -> m ()
setSearchBarVisible window v = liftIO $ SearchBar.setVisible (searchBar window) v

setSearchMode :: (MonadIO m)
              => MainWindow
              -> SearchMode
              -> m ()
setSearchMode window v = liftIO $ SearchBar.setSearchMode (searchBar window) v
-}


{-addAccelGroup :: (MonadIO m) => MainWindow -> AccelGroup -> m ()-}
addAccelGroup w g = liftIO $ window w `windowAddAccelGroup` g

addFileMenuAccelerator f e = (f . fileMenu) `addAccel` e
addSolutionMenuAccelerator f e = (f . projectMenu) `addAccel` e
addSearchMenuAccelerator f e = (f . searchMenu) `addAccel` e
addNavigationMenuAccelerator f e = (f . navigationMenu) `addAccel` e

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

--addFindClickedEventAccelerator = findButton `addSearchMenuAccelerator` "activate"
--addNavigateClickedEventAccelerator = navigateButton `addSearchMenuAccelerator` "activate"
--addSearchClickedEventAccelerator = SearchBar.addSearchClickedEventAccelerator . searchBar
addGotoDeclarationEventAccelerator = gotoDeclarationButton `addSearchMenuAccelerator` "activate"
addBackEventAccelerator = backButton `addNavigationMenuAccelerator` "activate"
addForwardEventAccelerator = forwardButton `addNavigationMenuAccelerator` "activate"
