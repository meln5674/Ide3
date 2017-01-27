{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalMonad where

import Data.Text (Text)

import Distribution.PackageDescription


import Ide3.Types hiding (BuildInfo)


data CabalProjectInfo 
    = LibraryInfo
    | ExecutableInfo String
    | TestSuiteInfo String
    | BenchmarkInfo String
  deriving (Show)

libraryInfo :: ProjectInfo 
libraryInfo = ProjectInfo "library"


editBuildInfo :: CabalProject -> (BuildInfo -> BuildInfo) -> CabalProject
editBuildInfo (LibraryProject lib) f = LibraryProject lib{ libBuildInfo = f $ libBuildInfo lib }
editBuildInfo (ExecutableProject n exe) f = ExecutableProject n exe{ buildInfo = f $ buildInfo exe }
editBuildInfo (TestSuiteProject n test) f = TestSuiteProject n test{ testBuildInfo = f $ testBuildInfo test }
editBuildInfo (BenchmarkProject n bench) f = BenchmarkProject n bench{ benchmarkBuildInfo = f $ benchmarkBuildInfo bench }

withBuildInfo :: CabalProject -> (BuildInfo -> a) -> a
withBuildInfo (LibraryProject lib) f = f $ libBuildInfo lib
withBuildInfo (ExecutableProject _ exe) f = f $ buildInfo exe
withBuildInfo (TestSuiteProject _ test) f = f $ testBuildInfo test
withBuildInfo (BenchmarkProject _ bench) f = f $ benchmarkBuildInfo bench

-- | A solution type
data CabalProject
    = LibraryProject Library -- ^ A library project
    | ExecutableProject ProjectInfo Executable -- ^ An executable project
    | TestSuiteProject ProjectInfo TestSuite -- ^ A test suite project
    | BenchmarkProject ProjectInfo Benchmark

class Monad m => CabalMonad m where
    getCabalProjects :: SolutionResult u m [CabalProjectInfo] 
    getCabalProject :: CabalProjectInfo -> SolutionResult u m CabalProject
    getCabalProjectInfo :: ProjectInfo -> SolutionResult u m CabalProjectInfo
    lookupCabalProject :: ProjectInfo -> SolutionResult u m CabalProject
    addCabalProject :: CabalProjectInfo -> CabalProject -> SolutionResult u m ()
    updateCabalProject :: CabalProjectInfo -> CabalProject -> SolutionResult u m ()
    removeCabalProject :: CabalProjectInfo -> SolutionResult u m ()
    getPackageDescription :: SolutionResult u m PackageDescription
