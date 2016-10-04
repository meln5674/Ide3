{-# LANGUAGE NamedFieldPuns #-}
module Dialogs.NewProjectDialog
    ( NewProjectDialog
    ) where

{-
data NewProjectDialog
    = NewProjectDialog
    { window :: Window
    , primarySrcDirLabel :: Label
    , primarySrcDirBox :: Entry
    , secondarySrcDirsLabel :: Label
    , secondarySrcDirsBox :: Entry
    , projectTypeDropdown :: ComboBox
    , projectTypeStack :: Stack
    , executableView
    , libraryView
    , testSuiteView
    , benchmarkView
    }
-}


{-


drop down box with options for library, executable, test-suite, and benchmark

all 4 have fields for the primary source dir and secondary source dirs

executable, test-suite and benchmark have fields for the project name

executable and benchmark have a field for main module

test-suite has a drop-down for exitcode/detailed

test-suite in exitcode has a field for main module

test-sutie in detailed has a field for test-module

-}
