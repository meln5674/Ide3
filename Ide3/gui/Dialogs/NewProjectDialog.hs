{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Dialogs.NewProjectDialog
    ( NewProjectDialog
    , DialogMode(..)
    , make
    , setVisible
    , confirmClicked
    , cancelClicked
    , projectTypeChanged
    , testTypeChanged
    , resetFields
    , getPrimarySrcDir
    , setPrimarySrcDir
    , getDialogMode
    , setDialogMode
    , getSecondarySrcDirs
    , setSecondarySrcDirs
    , getDependencies
    , setDependencies
    , getProjectType
    , setProjectType
    , getExecutableProjectName
    , setExecutableProjectName
    , getExecutableMainModule
    , setExecutableMainModule
    , getTestProjectName
    , setTestProjectName
    , getTestProjectType
    , setTestProjectType
    , getTestMainModule
    , setTestMainModule
    , getTestTestModule
    , setTestTestModule
    , getBenchmarkProjectName
    , setBenchmarkProjectName
    , getBenchmarkMainModule
    , setBenchmarkMainModule
    , ProjectType(..)
    , TestSuiteType(..)
    ) where

import Data.Functor.Identity

import Data.Text (Text)
import qualified Data.Text as T


import Control.Concurrent.MVar

import Control.Monad
import Control.Monad.Trans

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import GI.Gdk hiding (Window)
import Data.GI.Gtk.ModelView.CellLayout
import Data.GI.Gtk.ModelView.SeqStore
import Data.GI.Base.Attributes

import GuiHelpers

type NewProjectDialogSignal subObject info = SubSignalProxy NewProjectDialog subObject info

confirmClicked :: NewProjectDialogSignal Button WidgetButtonPressEventSignalInfo
confirmClicked dialog = (confirmButton dialog, #buttonPressEvent) 

cancelClicked :: NewProjectDialogSignal Button WidgetButtonPressEventSignalInfo
cancelClicked dialog = (cancelButton dialog, #buttonPressEvent)

projectTypeChanged :: NewProjectDialogSignal ComboBox ComboBoxChangedSignalInfo
projectTypeChanged dialog = (projectTypeDropdown dialog, #changed)

testTypeChanged :: NewProjectDialogSignal ComboBox ComboBoxChangedSignalInfo
testTypeChanged dialog = (testTypeBox $ testSuiteView $ projectTypeView dialog, #changed)

getDialogMode :: MonadIO m => NewProjectDialog -> m DialogMode
getDialogMode = liftIO . readMVar . dialogMode

setDialogMode :: MonadIO m => NewProjectDialog -> DialogMode -> m ()
setDialogMode dialog mode = do
    void $ liftIO $ swapMVar (dialogMode dialog) mode
    liftIO $ print mode

resetFields :: MonadIO m => NewProjectDialog -> m ()
resetFields dialog = do
    let buffers = map ($ dialog)
            [ primarySrcDirBuffer
            , secondarySrcDirsBuffer
            , dependenciesBuffer
            , exeProjectNameBuffer . executableView . projectTypeView
            , exeMainModuleBuffer . executableView . projectTypeView
            , testProjectNameBuffer . testSuiteView . projectTypeView
            , testMainModuleBuffer . testExitCodeView . testTypeView . testSuiteView . projectTypeView
            , testTestModuleBuffer . testDetailedView . testTypeView . testSuiteView . projectTypeView
            , benchProjectNameBuffer . benchmarkView . projectTypeView
            , benchMainModuleBuffer . benchmarkView . projectTypeView
            ]
    forM_ buffers $ flip setEntryBufferText ""

getPrimarySrcDir :: MonadIO m => NewProjectDialog -> m Text
getPrimarySrcDir = getEntryBufferText . primarySrcDirBuffer

setPrimarySrcDir :: MonadIO m => NewProjectDialog -> Text -> m ()
setPrimarySrcDir = setEntryBufferText . primarySrcDirBuffer

getSecondarySrcDirs :: MonadIO m => NewProjectDialog -> m Text
getSecondarySrcDirs = getEntryBufferText . secondarySrcDirsBuffer

setSecondarySrcDirs :: MonadIO m => NewProjectDialog -> Text -> m ()
setSecondarySrcDirs = setEntryBufferText . secondarySrcDirsBuffer

getDependencies :: MonadIO m => NewProjectDialog -> m Text
getDependencies = getEntryBufferText . dependenciesBuffer

setDependencies :: MonadIO m => NewProjectDialog -> Text -> m ()
setDependencies = setEntryBufferText . dependenciesBuffer

getProjectType :: MonadIO m => NewProjectDialog -> m ProjectType
getProjectType dialog 
    = liftM (toEnum . fromIntegral)
    $ comboBoxGetActive 
    $ projectTypeDropdown dialog

setProjectType :: MonadIO m => NewProjectDialog -> ProjectType -> m ()
setProjectType dialog 
    = comboBoxSetActive (projectTypeDropdown dialog) 
    . fromIntegral . fromEnum 
        
    
getExecutableProjectName :: MonadIO m => NewProjectDialog -> m Text 
getExecutableProjectName
    = getEntryBufferText 
    . exeProjectNameBuffer
    . executableView 
    . projectTypeView

setExecutableProjectName :: MonadIO m => NewProjectDialog -> Text -> m ()
setExecutableProjectName
    = setEntryBufferText
    . exeProjectNameBuffer
    . executableView
    . projectTypeView
      

getExecutableMainModule :: MonadIO m => NewProjectDialog -> m Text
getExecutableMainModule 
    = getEntryBufferText 
    . exeMainModuleBuffer
    . executableView 
    . projectTypeView

setExecutableMainModule :: MonadIO m => NewProjectDialog -> Text -> m ()
setExecutableMainModule 
    = setEntryBufferText 
    .  exeMainModuleBuffer
    . executableView 
    . projectTypeView

getTestProjectName :: MonadIO m => NewProjectDialog -> m Text
getTestProjectName
    = getEntryBufferText
    . testProjectNameBuffer
    . testSuiteView
    . projectTypeView

setTestProjectName :: MonadIO m => NewProjectDialog -> Text -> m ()
setTestProjectName
    = setEntryBufferText
    . testProjectNameBuffer
    . testSuiteView
    . projectTypeView

getTestProjectType :: MonadIO m => NewProjectDialog -> m TestSuiteType
getTestProjectType dialog = do
    ix <- comboBoxGetActive 
        $ testTypeBox
        $ testSuiteView 
        $ projectTypeView dialog
    seqStoreGetValue
        ( testTypeModel
        $ testSuiteView
        $ projectTypeView dialog
        )
        ix

setTestProjectType :: MonadIO m => NewProjectDialog -> TestSuiteType -> m ()
setTestProjectType dialog
    = stackSetVisibleChildName (testTypeStack $ testTypeView $ testSuiteView $ projectTypeView dialog)
    . testTypeName

getTestMainModule :: MonadIO m => NewProjectDialog -> m Text
getTestMainModule
    = getEntryBufferText
    . testMainModuleBuffer
    . testExitCodeView
    . testTypeView
    . testSuiteView
    . projectTypeView

setTestMainModule :: MonadIO m => NewProjectDialog -> Text -> m ()
setTestMainModule
    = setEntryBufferText
    . testMainModuleBuffer
    . testExitCodeView
    . testTypeView
    . testSuiteView
    . projectTypeView

getTestTestModule :: MonadIO m => NewProjectDialog -> m Text
getTestTestModule
    = getEntryBufferText
    . testTestModuleBuffer
    . testDetailedView
    . testTypeView
    . testSuiteView
    . projectTypeView

setTestTestModule :: MonadIO m => NewProjectDialog -> Text -> m ()
setTestTestModule
    = setEntryBufferText
    . testTestModuleBuffer
    . testDetailedView
    . testTypeView
    . testSuiteView
    . projectTypeView

getBenchmarkProjectName :: MonadIO m => NewProjectDialog -> m Text
getBenchmarkProjectName
    = getEntryBufferText
    . benchProjectNameBuffer
    . benchmarkView
    . projectTypeView

setBenchmarkProjectName :: MonadIO m => NewProjectDialog -> Text -> m ()
setBenchmarkProjectName
    = setEntryBufferText
    . benchProjectNameBuffer
    . benchmarkView
    . projectTypeView

getBenchmarkMainModule :: MonadIO m => NewProjectDialog -> m Text
getBenchmarkMainModule
    = getEntryBufferText
    . benchMainModuleBuffer
    . benchmarkView
    . projectTypeView

setBenchmarkMainModule :: MonadIO m => NewProjectDialog -> Text -> m ()
setBenchmarkMainModule
    = setEntryBufferText
    . benchMainModuleBuffer
    . benchmarkView
    . projectTypeView

data DialogMode = CreateProject | EditProject Text deriving Show

data NewProjectDialog
    = NewProjectDialog
    { window :: Window
    , dialogMode :: MVar DialogMode
    , primarySrcDirLabel :: Label
    , primarySrcDirBuffer :: EntryBuffer
    , primarySrcDirBox :: Entry
    , secondarySrcDirsLabel :: Label
    , secondarySrcDirsBuffer :: EntryBuffer
    , secondarySrcDirsBox :: Entry
    , dependenciesLabel :: Label
    , dependenciesBox :: Entry
    , dependenciesBuffer :: EntryBuffer
    , projectTypeLabel :: Label
    , projectTypeDropdown :: ComboBox
    , projectTypeModel :: SeqStore ProjectType
    , projectTypeView :: ProjectTypeView
    , confirmButton :: Button
    , cancelButton :: Button
    }

data ProjectTypeView
    = ProjectTypeView
    { projectTypeStack :: Stack
    , executableView :: ExecutableView
    , libraryView :: LibraryView
    , testSuiteView :: TestSuiteView
    , benchmarkView :: BenchmarkView
    }

data ProjectType = Executable | Library | TestSuite | Benchmark deriving (Eq,Ord,Enum)
data TestSuiteType = ExitCode | Detailed deriving (Eq,Ord,Enum)

data ExecutableView 
    = ExecutableView
    { exeProjectNameLabel :: Label
    , exeProjectNameBox :: Entry
    , exeProjectNameBuffer :: EntryBuffer
    , exeMainModuleLabel :: Label
    , exeMainModuleBox :: Entry
    , exeMainModuleBuffer :: EntryBuffer
    }

data LibraryView 
    = LibraryView
    { }
        
data TestSuiteView 
    = TestSuiteView
    { testProjectNameLabel :: Label
    , testProjectNameBox :: Entry
    , testProjectNameBuffer :: EntryBuffer
    , testTypeLabel :: Label
    , testTypeBox :: ComboBox
    , testTypeModel :: SeqStore TestSuiteType
    , testTypeView :: TestTypeView
    }

data BenchmarkView 
    = BenchmarkView
    { benchProjectNameLabel :: Label
    , benchProjectNameBox :: Entry
    , benchProjectNameBuffer :: EntryBuffer
    , benchMainModuleLabel :: Label
    , benchMainModuleBox :: Entry
    , benchMainModuleBuffer :: EntryBuffer
    }

data TestTypeView
    = TestTypeView
    { testTypeStack :: Stack
    , testExitCodeView :: TestSuiteExitCodeView
    , testDetailedView :: TestSuiteDetailedView
    }

data TestSuiteExitCodeView
    = TestSuiteExitCodeView
    { testMainModuleLabel :: Label
    , testMainModuleBox :: Entry
    , testMainModuleBuffer :: EntryBuffer
    }

data TestSuiteDetailedView
    = TestSuiteDetailedView
    { testTestModuleLabel :: Label
    , testTestModuleBox :: Entry
    , testTestModuleBuffer :: EntryBuffer
    }


projectTypeName :: ProjectType -> Text
projectTypeName Executable = exeViewChildName
projectTypeName Library = libViewChildName
projectTypeName TestSuite = testViewChildName
projectTypeName Benchmark = benchViewChildName

testTypeName :: TestSuiteType -> Text
testTypeName ExitCode = exitcodeViewChildName
testTypeName Detailed = detailedViewChildName

renderProjectType :: (AttrSetC info o "text" Text, IsCellRendererText o) 
                  => ProjectType -> [AttrOp o AttrSet]
renderProjectType type_ = [#text := projectTypeName type_]

renderTestType :: (AttrSetC info o "text" Text, IsCellRendererText o) 
                  => TestSuiteType -> [AttrOp o AttrSet]
renderTestType type_ = [#text := testTypeName type_]




make :: ( MonadIO m )
     => ( NewProjectDialog -> m b) -> m b
make f = makeWindowWith 
    $ \window -> makeVBoxWith window $ \vbox -> do
        (primarySrcDirLabel, primarySrcDirBox, primarySrcDirBuffer) <- 
            makePrimarySrcDirBoxWith vbox 
                $ \hbox -> makeLabelEntryPairWith "Primary Source Directory" hbox
                $ \a b c -> return (a,b,c)
        (secondarySrcDirsLabel, secondarySrcDirsBox, secondarySrcDirsBuffer) <- 
            makeSecondarySrcDirsBoxWith vbox 
                $ \hbox -> makeLabelEntryPairWith "Secondary Source Directories" hbox
                $ \a b c -> return (a,b,c)
        (dependenciesLabel, dependenciesBox, dependenciesBuffer) <- 
            makeDependenciesBoxWith vbox 
                $ \hbox -> makeLabelEntryPairWith "Dependencies" hbox
                $ \a b c -> return (a,b,c)
        (projectTypeLabel, projectTypeDropdown, projectTypeModel) <- 
            makeProjectTypeBoxWith vbox 
                $ \hbox -> makeLabelComboBoxPairWith "Project Type" hbox
                $ \a b c -> return (a,b,c)
        projectTypeView <- makeProjectTypeViewWith vbox return
        projectTypeRenderer <- cellRendererTextNew
        cellLayoutPackStart projectTypeDropdown projectTypeRenderer True
        cellLayoutSetAttributes projectTypeDropdown 
                                projectTypeRenderer 
                                projectTypeModel 
                                renderProjectType
        mapM (seqStoreAppend projectTypeModel) 
            [ Executable
            , Library
            , TestSuite
            , Benchmark
            ]
        comboBoxSetActive projectTypeDropdown 0
        (confirmButton,cancelButton) <- makeHBoxWith vbox $ \hbox -> do
            confirmButton <- makeButton "Confirm" hbox
            cancelButton <- makeButton "Cancel" hbox
            return (confirmButton, cancelButton)
        dialogMode <- liftIO $ newMVar CreateProject
        projectTypeDropdown `GI.Gtk.on` #changed $ do
            comboBoxGetActive projectTypeDropdown
            >>= seqStoreGetValue projectTypeModel
            >>= stackSetVisibleChildName 
                (projectTypeStack projectTypeView)
                . projectTypeName
        (testTypeBox $ testSuiteView projectTypeView) `GI.Gtk.on` #changed $ do
            comboBoxGetActive (testTypeBox $ testSuiteView projectTypeView) 
            >>= seqStoreGetValue (testTypeModel $ testSuiteView projectTypeView)
            >>= stackSetVisibleChildName
                (testTypeStack $ testTypeView $ testSuiteView projectTypeView)
                . testTypeName
        window `GI.Gtk.on` #deleteEvent $ \_ -> widgetHideOnDelete window
        f NewProjectDialog
            { window
            , dialogMode
            , primarySrcDirLabel
            , primarySrcDirBox
            , primarySrcDirBuffer
            , secondarySrcDirsLabel
            , secondarySrcDirsBox
            , secondarySrcDirsBuffer
            , dependenciesLabel
            , dependenciesBox
            , dependenciesBuffer
            , projectTypeLabel
            , projectTypeDropdown
            , projectTypeModel
            , projectTypeView
            , confirmButton
            , cancelButton
            }


makePrimarySrcDirBoxWith :: ( MonadIO m
                            , IsContainer self
                            )
                         => self
                         -> ( HBox -> m b )
                         -> m b
makePrimarySrcDirBoxWith = makeHBoxWith
    
makeSecondarySrcDirsBoxWith :: ( MonadIO m
                               , IsContainer self
                               )
                            => self
                            -> ( HBox -> m b )
                            -> m b
makeSecondarySrcDirsBoxWith = makeHBoxWith

makeDependenciesBoxWith :: ( MonadIO m
                               , IsContainer self
                               )
                            => self
                            -> ( HBox -> m b )
                            -> m b
makeDependenciesBoxWith = makeHBoxWith



makeProjectTypeBoxWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( HBox -> m b )
                       -> m b
makeProjectTypeBoxWith = makeHBoxWith

exeViewChildName :: Text
exeViewChildName = "Executable"

libViewChildName :: Text
libViewChildName = "Library"

testViewChildName :: Text
testViewChildName = "Test Suite"

benchViewChildName :: Text
benchViewChildName = "Benchmark"

makeProjectTypeViewWith :: ( MonadIO m
                           , IsContainer self
                           )
                        => self
                        -> ( ProjectTypeView -> m b)
                        -> m b
makeProjectTypeViewWith self f = do
    makeStackWith self $ \stack -> do
        executableView <- makeStackChildWith stack exeViewChildName
            $ \exeBox -> makeExecutableViewWith exeBox return
        libraryView <- makeStackChildWith stack libViewChildName
            $ \libBox -> makeLibraryViewWith libBox return
        testSuiteView <- makeStackChildWith stack testViewChildName 
            $ \testBox -> makeTestSuiteViewWith testBox return
        benchmarkView <- makeStackChildWith stack benchViewChildName
            $ \benchBox -> makeBenchmarkViewWith benchBox return
        stackSetVisibleChildName stack exeViewChildName
        f ProjectTypeView
            { projectTypeStack = stack
            , executableView
            , libraryView
            , testSuiteView
            , benchmarkView
            }

makeExecutableViewWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( ExecutableView -> m b )
                       -> m b
makeExecutableViewWith self f = do
    (exeProjectNameLabel, exeProjectNameBox, exeProjectNameBuffer) <- 
        makeLabelEntryPairWith "Project Name" self $ \a b c -> return (a,b,c)
    (exeMainModuleLabel, exeMainModuleBox, exeMainModuleBuffer) <- 
        makeLabelEntryPairWith "Main Module" self $ \a b c -> return (a,b,c)
    f ExecutableView
        { exeProjectNameLabel
        , exeProjectNameBox
        , exeProjectNameBuffer
        , exeMainModuleLabel
        , exeMainModuleBox
        , exeMainModuleBuffer
        }

makeLibraryViewWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( LibraryView -> m b )
                       -> m b
makeLibraryViewWith self f = f LibraryView
    
exitcodeViewChildName :: Text
exitcodeViewChildName = "Exit Code"

detailedViewChildName :: Text
detailedViewChildName = "Detailed"

makeTestSuiteViewWith :: ( MonadIO m
                         , IsContainer self
                         )
                      => self
                      -> ( TestSuiteView -> m b )
                      -> m b
makeTestSuiteViewWith self f = do
    (testProjectNameLabel, testProjectNameBox, testProjectNameBuffer) <-
        makeLabelEntryPairWith "Project Name" self $ \a b c -> return (a,b,c)
    (testTypeLabel, testTypeBox, testTypeModel) <-
        makeLabelComboBoxPairWith "Test Type" self $ \a b c -> return (a,b,c)
    testTypeRenderer <- cellRendererTextNew
    cellLayoutPackStart testTypeBox testTypeRenderer True
    cellLayoutSetAttributes testTypeBox
                            testTypeRenderer 
                            testTypeModel 
                            renderTestType
    mapM (seqStoreAppend testTypeModel)
        [ ExitCode
        , Detailed
        ]
    comboBoxSetActive testTypeBox 0
    testTypeView <- makeTestTypeViewWith self return
    f TestSuiteView
            { testProjectNameLabel
            , testProjectNameBox
            , testProjectNameBuffer
            , testTypeLabel
            , testTypeBox
            , testTypeModel
            , testTypeView
            }
        
        
makeTestTypeViewWith :: ( MonadIO m
                         , IsContainer self
                         )
                      => self
                      -> ( TestTypeView -> m b )
                      -> m b
makeTestTypeViewWith self f = do
    makeStackWith self $ \stack -> do
        testExitCodeView <- makeStackChildWith stack exitcodeViewChildName
            $ \exitcodeBox-> makeTestSuiteExitCodeViewWith exitcodeBox return
        testDetailedView <- makeStackChildWith stack detailedViewChildName
            $ \detailedBox -> makeTestSuiteDetailedViewWith detailedBox return
        stackSetVisibleChildName stack exitcodeViewChildName
        f TestTypeView
            { testTypeStack = stack
            , testExitCodeView
            , testDetailedView
            }

makeBenchmarkViewWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( BenchmarkView -> m b )
                       -> m b
makeBenchmarkViewWith self f = do
    (benchProjectNameLabel, benchProjectNameBox, benchProjectNameBuffer) <- 
        makeLabelEntryPairWith "Project Name" self $ \a b c -> return (a,b,c)
    (benchMainModuleLabel, benchMainModuleBox, benchMainModuleBuffer) <- 
        makeLabelEntryPairWith "Main Module" self $ \a b c -> return (a,b,c)
    f BenchmarkView
        { benchProjectNameLabel
        , benchProjectNameBox
        , benchProjectNameBuffer
        , benchMainModuleLabel
        , benchMainModuleBox
        , benchMainModuleBuffer
        }

makeTestSuiteExitCodeViewWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( TestSuiteExitCodeView -> m b )
                       -> m b
makeTestSuiteExitCodeViewWith self f = do
    (testMainModuleLabel, testMainModuleBox, testMainModuleBuffer) <- 
        makeLabelEntryPairWith "Main Module" self $ \a b c -> return (a,b,c)
    f TestSuiteExitCodeView
        { testMainModuleLabel
        , testMainModuleBox
        , testMainModuleBuffer
        }


makeTestSuiteDetailedViewWith :: ( MonadIO m
                          , IsContainer self
                          )
                       => self
                       -> ( TestSuiteDetailedView -> m b )
                       -> m b
makeTestSuiteDetailedViewWith self f = do
    (testTestModuleLabel, testTestModuleBox, testTestModuleBuffer) <- 
        makeLabelEntryPairWith "Test Module" self $ \a b c -> return (a,b,c)
    f TestSuiteDetailedView
        { testTestModuleLabel
        , testTestModuleBox
        , testTestModuleBuffer
        }

{-


drop down box with options for library, executable, test-suite, and benchmark

all 4 have fields for the primary source dir and secondary source dirs

executable, test-suite, and benchmark have fields for the project name

executable and benchmark have a field for main module

test-suite has a drop-down for exitcode/detailed

test-suite in exitcode has a field for main module

test-sutie in detailed has a field for test-module

-}

setVisible :: MonadIO m => NewProjectDialog -> Bool -> m ()
setVisible dialog v = set (window dialog) [widgetVisible := v]
