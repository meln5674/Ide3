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

import Data.Monoid

import Data.Text hiding (map)

import Control.Monad
import Control.Monad.Trans

import Data.GI.Base.Attributes
import GI.Gtk hiding (TreePath, SearchBar)
import GI.Gdk hiding (Window, windowNew)
import Data.GI.Gtk.ModelView.CellLayout

import Ide3.Types

import BetterTextView

import ErrorParser.Types

import GuiClass.Types

import GuiEnv
import GuiMonad

import GuiHelpers

import Dialogs.SearchBar (SearchBar)
import qualified Dialogs.SearchBar as SearchBar

import SearchMode
import DeclarationPath

import GuiClass (SolutionTreeElem (..))

-- | ADT for the main application window
data MainWindow
    = MainWindow
    { window :: Window
    , fileMenu :: FileMenu
    , solutionMenu :: SolutionMenu
    , searchMenu :: SearchMenu
    , navigationMenu :: NavigationMenu
    , solutionViewer :: SolutionViewer
    , buildViewer :: BuildViewer
    , searchBar :: SearchBar
    }  


-- | Renderer for the solution tree
renderSolutionTreeElem :: (AttrSetC info o "text" Text) 
                       => SolutionTreeElem 
                       -> [AttrOp o 'AttrSet]
renderSolutionTreeElem (ProjectElem (ProjectInfo n)) = [#text := n]
renderSolutionTreeElem (ModuleElem (ModuleInfo (Symbol s)) _) = [#text := s]
renderSolutionTreeElem (ModuleElem (UnamedModule (Just path)) _) = [#text := pack path]
renderSolutionTreeElem (ModuleElem (UnamedModule Nothing) _) = [#text := ("???" :: Text)]
renderSolutionTreeElem (DeclElem (SymbolDeclarationInfo sym))
    = [#text := getSymbol sym]
renderSolutionTreeElem (DeclElem (RawDeclarationInfo s))
    = [#text := s]
renderSolutionTreeElem ImportsElem = [#text := ("Imports" :: Text)]
renderSolutionTreeElem ExportsElem = [#text := ("Exports" :: Text)]
renderSolutionTreeElem PragmasElem = [#text := ("Pragmas" :: Text)]
renderSolutionTreeElem (ImportElem _ (WithBody _ importBody)) = [#text := importBody] 
renderSolutionTreeElem (ExportElem _ (WithBody _ exportBody)) = [#text := exportBody] 
renderSolutionTreeElem (PragmaElem p) = [#text := p]
renderSolutionTreeElem (UnparsableModuleElem (ModuleInfo (Symbol s)) _ _)
    = [#text := (s <> " (UNPARSEABLE)" :: Text)]
renderSolutionTreeElem (UnparsableModuleElem (UnamedModule (Just path)) _ _)
    = [#text := (pack path <> " (UNPARSEABLE)" :: Text)]
renderSolutionTreeElem (UnparsableModuleElem (UnamedModule Nothing) _ _)
    = [#text := ("??? (UNPARSEABLE)" :: Text)]
renderSolutionTreeElem SolutionElem = [#text := ("THIS SHOULDN'T BE SEEN" :: Text)]


-- | Renderer for the error list image column
renderImageCell :: (AttrSetC info o "stockId" Text) 
                => Error ItemPath Text
                -> [AttrOp o 'AttrSet]
renderImageCell (Warning _ _ _ _) = [#stockId := STOCK_DIALOG_WARNING]
renderImageCell (Error _ _ _ _) = [#stockId := STOCK_DIALOG_ERROR]

-- | Renderer for the error list project column
renderProjectCell :: (AttrSetC info o "text" Text)
                  => Error ItemPath Text
                  -> [AttrOp o 'AttrSet]
renderProjectCell e = [#text := unProjectInfo pji]
  where
    (ProjectChild pji _) = errorLocation e

-- | Renderer for the error list module column
renderModuleCell :: (AttrSetC info o "text" Text) 
                 => Error ItemPath Text
                 -> [AttrOp o 'AttrSet]
renderModuleCell e = [#text := moduleInfoString mi "???"]
  where
    (ProjectChild _ (ModuleChild mi _)) = errorLocation e

-- | Renderer for the error list declaration column
renderDeclarationCell :: (AttrSetC info o "text" Text)
              => Error ItemPath Text
              -> [AttrOp o 'AttrSet]
renderDeclarationCell err = [#text := text]
  where
    (ProjectChild _ (ModuleChild _ x)) = errorLocation err
    text = case x of
        Just (HeaderCommentString _) -> "[MODULE HEADER]"
        Just (PragmaString p) -> "[PRAGMA] " <> p
        Just (ImportString i) -> body i
        Just (ExportString e) -> "[EXPORT] " <> body e
        Just (DeclarationString di) -> case item di of
            SymbolDeclarationInfo sym -> getSymbol sym
            RawDeclarationInfo text -> text
        Nothing -> ""

-- | Renderer for the error list row column
renderRowCell :: (AttrSetC info o "text" Text) 
              => Error ItemPath Text
              -> [AttrOp o 'AttrSet]
renderRowCell (Warning _ row _ _) = [#text := pack (show row)]
renderRowCell (Error _ row _ _) = [#text := pack (show row)]

-- | Renderer for the error list column column
renderColumnCell :: (AttrSetC info o "text" Text)
                 => Error ItemPath Text
                 -> [AttrOp o 'AttrSet]
renderColumnCell (Warning _ _ col _) = [#text := pack (show col)]
renderColumnCell (Error _ _ col _) = [#text := pack (show col)]

-- | Renderer for the error list message column
renderMessageCell :: (AttrSetC info o "text" Text) 
                  => Error ItemPath Text
                  -> [AttrOp o 'AttrSet]
renderMessageCell (Warning _ _ _ msg) = [#text := msg]
renderMessageCell (Error _ _ _ msg) = [#text := msg]



-- | Create the main application window
make :: (MonadIO m)
     => (MainWindow -> GuiEnvT {-proxy-} m' p  m a) 
     -> GuiEnvT {-proxy-} m' p  m a
make f = makeMainWindowWith $ \window -> do
    renderer <- makeRenderer
    makeOverlayWith window $ \overlay -> do
        searchBarBox <- vBoxNew False 0
        --overlay `overlayAddOverlay` searchBarBox
        --overlay `containerAdd` searchBarBox
        overlaySetOverlayPassThrough overlay searchBarBox True
        searchBar <- SearchBar.make searchBarBox
        SearchBar.setVisible searchBar False
        makeVBoxWith overlay $ \container -> do
            menuBar <- makeMainMenuBar container
            fileMenu <- makeFileMenu menuBar
            solutionMenu <- makeSolutionMenu menuBar
            searchMenu <- makeSearchMenu menuBar
            navigationMenu <- makeNavigationMenu menuBar
            makeVPanedWith container $ \vbox -> do
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
                  , fileMenu
                  , solutionMenu
                  , searchMenu
                  , solutionViewer
                  , navigationMenu
                  , buildViewer
                  , searchBar
                  }

makeRenderer :: (MonadIO m) => GuiEnvT {-proxy-} m' p  m CellRendererText
makeRenderer = cellRendererTextNew

makeSoloBox :: (MonadIO m)
            => GuiEnvT {-proxy-} m' p  m VBox
makeSoloBox = vBoxNew False 0


makeVPanedWith :: (MonadIO m, IsContainer self) 
               => self 
               -> (VPaned -> GuiEnvT {-proxy-} m' p  m b) 
               -> GuiEnvT {-proxy-} m' p  m b
makeVPanedWith container f = do
    vbox <- vPanedNew
    container `containerAdd` vbox
    f vbox

makeHPanedWith :: (MonadIO m, IsContainer self) 
               => self 
               -> (HPaned -> GuiEnvT {-proxy-} m' p  m b) 
               -> GuiEnvT {-proxy-} m' p  m b
makeHPanedWith container f = do
    hbox <- hPanedNew
    container `containerAdd` hbox
    f hbox

{-
makeContainerWith :: (MonadIO m, IsBox self) => self -> (Table -> GuiEnvT {-proxy-} m' p  m b) -> GuiEnvT {-proxy-} m' p  m b
makeContainerWith vbox f = do
    container <- tableNew 2 3 False
    boxPackEnd vbox container True True 0
    f container
-}

makeMainMenuBar :: (MonadIO m, IsBox self) => self -> GuiEnvT {-proxy-} m' p  m MenuBar
makeMainMenuBar vbox = do
    menuBar <- menuBarNew
    boxPackStart vbox menuBar False False 0
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
makeSolutionMenu = makeSolutionMenuWith $ \solutionMenu -> do
    buildButton <- makeBuildButton solutionMenu
    runButton <- makeRunButton solutionMenu
    return SolutionMenu
           { buildButton
           , runButton
           }

makeSolutionMenuWith :: (MonadIO m) => (Menu -> GuiEnvT {-proxy-} m' p  m b) -> MenuBar -> GuiEnvT {-proxy-} m' p  m b
makeSolutionMenuWith = makeMenuWith "Solution"

makeBuildButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBuildButton = makeMenuButton "Build"

makeRunButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
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

makeFindButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeFindButton = makeMenuButton "Find"

makeNavigateButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeNavigateButton = makeMenuButton "Navigate"

makeGotoDeclarationButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
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

makeBackButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeBackButton = makeMenuButton "Back"

makeForwardButton :: (MonadIO m, IsMenuShell self) => self -> GuiEnvT {-proxy-} m' p  m MenuItem
makeForwardButton = makeMenuButton "Forward"

data SolutionViewer
    = SolutionViewer
    { projectView :: TreeView
    , declView :: BetterTextView
    , declBuffer :: TextBuffer
    }

makeSolutionViewer :: (MonadIO m
                     , IsContainer self
                     , IsCellRenderer cell
                     ) 
                  => cell
                  -> (SolutionTreeElem -> [AttrOp cell 'AttrSet])
                  -> self
                  -> GuiEnvT {-proxy-} m' p  m SolutionViewer
makeSolutionViewer renderer renderFunc container = do
    makeHPanedWith container $ \hbox -> do
        projectViewBox <- makeSoloBox
        declViewBox <- makeSoloBox
        hbox `panedAdd1` projectViewBox
        hbox `panedAdd2` declViewBox
        projectView <- makeProjView renderer projectViewBox renderFunc
        declView <- makeDeclView declViewBox
        declBuffer <- withGuiComponents $ flip withEditorBuffer return
        return SolutionViewer
               { projectView
               , declView
               , declBuffer
               }

makeProjView :: ( MonadIO m
                , IsContainer self
                , IsCellRenderer cell
                ) 
             => cell 
             -> self 
             -> (SolutionTreeElem -> [AttrOp cell 'AttrSet]) 
             -> GuiEnvT {-proxy-} m' p  m TreeView
makeProjView renderer container renderFunc = makeScrolledWindowWith container $ \scrollWindow -> do
    treeViewColumn <- treeViewColumnNew
    projView <- withGuiComponents $ flip withSolutionTree treeViewNewWithModel
    index <- treeViewAppendColumn projView treeViewColumn
    cellLayoutPackStart treeViewColumn renderer False
    withGuiComponents $ flip withSolutionTree $ \treeStore -> 
        cellLayoutSetAttributes treeViewColumn renderer treeStore renderFunc
    scrollWindow `containerAdd` projView
    --projView `treeViewSetTooltipColumn` index
    set projView [#hasTooltip := True]
    return projView

makeDeclView :: ( MonadIO m
                , IsContainer self
                ) 
             => self -> GuiEnvT {-proxy-} m' p  m BetterTextView
makeDeclView container = makeScrolledWindowWith container $ \scrollWindow -> do
    declView <- withGuiComponents $ flip withEditorBuffer $ betterTextViewNewWithBuffer
    setSub declView [ mkBTVAttr (#monospace := True) ]
    scrollWindow `containerAdd` declView
    return declView

data BuildViewer
    = BuildViewer
    { buildView :: TextView
    , errorView :: TreeView
    }

makeBuildViewer :: ( MonadIO m
                   , IsContainer self
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
                 , IsContainer self
                 ) 
              => self -> GuiEnvT {-proxy-} m' p  m TextView
makeBuildView container = makeScrolledWindowWith container $ \scrollWindow -> do
    buildView <- withGuiComponents $ flip withBuildBuffer textViewNewWithBuffer
    set buildView [ #editable := False ]
    scrollWindow `containerAdd` buildView
    return buildView


makeErrorView :: ( MonadIO m
                 , IsContainer self
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
            
            
            void $ treeViewAppendColumn errorView imageColumn
            void $ treeViewAppendColumn errorView projectColumn
            void $ treeViewAppendColumn errorView moduleColumn
            void $ treeViewAppendColumn errorView declarationColumn
            void $ treeViewAppendColumn errorView rowColumn
            void $ treeViewAppendColumn errorView columnColumn
            void $ treeViewAppendColumn errorView messageColumn
            
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
    window <- new Window []
    r <- f window
    liftIO $ widgetShowAll window
    return r

type MainWindowSignal object info = SubSignalProxy MainWindow object info

mkMainWindowSignal :: (MainWindow -> object) -> SignalProxy object info -> MainWindowSignal object info
mkMainWindowSignal getter signal window = (getter window, signal)

mkFileMenuSignal :: (FileMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkFileMenuSignal getter signal window = (getter $ fileMenu window, signal)

mkSolutionMenuSignal :: (SolutionMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSolutionMenuSignal getter signal window = (getter $ solutionMenu window, signal)

mkSearchMenuSignal :: (SearchMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSearchMenuSignal getter signal window = (getter $ searchMenu window, signal)

mkNavigationMenuSignal :: (NavigationMenu -> object) -> SignalProxy object info -> MainWindowSignal object info
mkNavigationMenuSignal getter signal window = (getter $ navigationMenu window, signal)

mkSolutionViewerSignal :: (SolutionViewer -> object) -> SignalProxy object info -> MainWindowSignal object info
mkSolutionViewerSignal getter signal window = (getter $ solutionViewer window, signal)

mkBuildViewerSignal :: (BuildViewer -> object) -> SignalProxy object info -> MainWindowSignal object info
mkBuildViewerSignal getter signal window = (getter $ buildViewer window, signal)

newClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
newClickedEvent = newButton `mkFileMenuSignal` #activate

openClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
openClickedEvent = openButton `mkFileMenuSignal` #activate

digestClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
digestClickedEvent = digestButton `mkFileMenuSignal` #activate

saveClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
saveClickedEvent = saveButton `mkFileMenuSignal` #activate

saveSolutionClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
saveSolutionClickedEvent = saveSolutionButton `mkFileMenuSignal` #activate

buildClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
buildClickedEvent = buildButton `mkSolutionMenuSignal` #activate

runClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
runClickedEvent = runButton `mkSolutionMenuSignal` #activate

declClickedEvent :: MainWindowSignal TreeView TreeViewRowActivatedSignalInfo
declClickedEvent = projectView `mkSolutionViewerSignal` #rowActivated
    
projectViewClickedEvent :: MainWindowSignal TreeView WidgetButtonPressEventSignalInfo
projectViewClickedEvent = projectView `mkSolutionViewerSignal` #buttonPressEvent

errorClickedEvent :: MainWindowSignal TreeView TreeViewRowActivatedSignalInfo
errorClickedEvent = errorView `mkBuildViewerSignal` #rowActivated

windowClosedEvent :: MainWindowSignal Window WidgetDestroySignalInfo
windowClosedEvent = window `mkMainWindowSignal` #destroy

findClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
findClickedEvent = findButton `mkSearchMenuSignal` #activate

navigateClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
navigateClickedEvent = navigateButton `mkSearchMenuSignal` #activate

gotoDeclarationClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
gotoDeclarationClickedEvent = gotoDeclarationButton `mkSearchMenuSignal` #activate

backClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
backClickedEvent = backButton `mkNavigationMenuSignal` #activate

forwardClickedEvent :: MainWindowSignal MenuItem MenuItemActivateSignalInfo
forwardClickedEvent = forwardButton `mkNavigationMenuSignal` #activate

declarationEditedEvent :: MainWindowSignal TextBuffer TextBufferChangedSignalInfo
declarationEditedEvent = declBuffer `mkSolutionViewerSignal` #changed

solutionTreeQueryTooltipEvent :: MainWindowSignal TreeView WidgetQueryTooltipSignalInfo
solutionTreeQueryTooltipEvent = projectView `mkSolutionViewerSignal` #queryTooltip

{-
searchClickedEvent :: (Monad m)
                   => MainWindowSignal proxy m' p  m Button IO ()
searchClickedEvent = wrapGuiEnvSignal searchBar SearchBar.searchClickedEvent
-}


data SolutionPathCoords
    = BinWindowCoords Int Int
    | WidgetCoords Int Int

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

data FocusTarget
    = FocusEditor
    | FocusLog
    | FocusErrorList
    | FocusSolutionTree

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

addAccelGroup :: (MonadIO m) => MainWindow -> AccelGroup -> m ()
addAccelGroup w g = liftIO $ window w `windowAddAccelGroup` g

addFileMenuAccelerator :: ( MonadIO m
                          , IsAccelGroup group
                          , Integral key
                          , IsWidget subObject
                          )
                       => (FileMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addFileMenuAccelerator f e = (f . fileMenu) `addAccel` e

addSolutionMenuAccelerator :: ( MonadIO m
                              , IsAccelGroup group
                              , Integral key
                              , IsWidget subObject
                              )
                       => (SolutionMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addSolutionMenuAccelerator f e = (f . solutionMenu) `addAccel` e

addSearchMenuAccelerator :: ( MonadIO m
                            , IsAccelGroup group
                            , Integral key
                            , IsWidget subObject
                            )
                       => (SearchMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addSearchMenuAccelerator f e = (f . searchMenu) `addAccel` e

addNavigationMenuAccelerator :: ( MonadIO m
                                , IsAccelGroup group
                                , Integral key
                                , IsWidget subObject
                                )
                       => (NavigationMenu -> subObject)
                       -> Text
                       -> MainWindow
                       -> group
                       -> key
                       -> [ModifierType] 
                       -> [AccelFlags]
                       -> m ()
addNavigationMenuAccelerator f e = (f . navigationMenu) `addAccel` e

addNewClickedEventAccelerator :: ( MonadIO m
                                 , IsAccelGroup group
                                 , Integral key
                                 )
                              => MainWindow
                              -> group
                              -> key
                              -> [ModifierType] 
                              -> [AccelFlags]
                              -> m ()
addNewClickedEventAccelerator = newButton `addFileMenuAccelerator` "activate"

addOpenClickedEventAccelerator :: ( MonadIO m
                                  , IsAccelGroup group
                                  , Integral key
                                  )
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addOpenClickedEventAccelerator = openButton `addFileMenuAccelerator` "activate"

addDigestClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addDigestClickedEventAccelerator = digestButton `addFileMenuAccelerator` "activate"

addSaveClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addSaveClickedEventAccelerator = saveButton `addFileMenuAccelerator` "activate"

addSaveSolutionClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                 -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addSaveSolutionClickedEventAccelerator = saveSolutionButton `addFileMenuAccelerator` "activate"

addBuildClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addBuildClickedEventAccelerator = buildButton `addSolutionMenuAccelerator` "activate"

addRunClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addRunClickedEventAccelerator = runButton `addSolutionMenuAccelerator` "activate"

--addFindClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addFindClickedEventAccelerator = findButton `addSearchMenuAccelerator` "activate"

--addNavigateClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addNavigateClickedEventAccelerator = navigateButton `addSearchMenuAccelerator` "activate"

--addSearchClickedEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
--                                  => MainWindow
--                                  -> group
--                                  -> key
--                                  -> [ModifierType] 
--                                  -> [AccelFlags]
--                                  -> m ()
--addSearchClickedEventAccelerator = SearchBar.addSearchClickedEventAccelerator . searchBar

addGotoDeclarationEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addGotoDeclarationEventAccelerator = gotoDeclarationButton `addSearchMenuAccelerator` "activate"

addBackEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addBackEventAccelerator = backButton `addNavigationMenuAccelerator` "activate"

addForwardEventAccelerator :: (MonadIO m, IsAccelGroup group, Integral key)
                                  => MainWindow
                                  -> group
                                  -> key
                                  -> [ModifierType] 
                                  -> [AccelFlags]
                                  -> m ()
addForwardEventAccelerator = forwardButton `addNavigationMenuAccelerator` "activate"


