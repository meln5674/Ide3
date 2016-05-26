module Tests.ImportExport (tests_importExport) where

import Test.HUnit

import Tests.ImportExport.Basic

tests_importExport = TestList
    [ tests_basicImportExport
    ]

