{-# LANGUAGE MultiParamTypeClasses #-}
module CabalMonad where

import Distribution.PackageDescription

import Ide3.Types hiding (BuildInfo)


data CabalProjectInfo 
    = LibraryInfo
    | ExecutableInfo String
    | TestSuiteInfo String
    | BenchmarkInfo String

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

class Monad m => CabalMonad m u where
    getCabalProjects :: SolutionResult m u [CabalProjectInfo] 
    getCabalProject :: CabalProjectInfo -> SolutionResult m u CabalProject
    getCabalProjectInfo :: ProjectInfo -> SolutionResult m u CabalProjectInfo
    lookupCabalProject :: ProjectInfo -> SolutionResult m u CabalProject
    addCabalProject :: CabalProjectInfo -> CabalProject -> SolutionResult m u ()
    updateCabalProject :: CabalProjectInfo -> CabalProject -> SolutionResult m u ()
    removeCabalProject :: CabalProjectInfo -> SolutionResult m u ()
