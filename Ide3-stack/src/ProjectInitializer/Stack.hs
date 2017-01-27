{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module ProjectInitializer.Stack where

import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text hiding (Text)

import Ide3.Types

import ProjectInitializer

import CabalMonad


import ProjectInitializer.Stack.Types
    

-- | An Initializer that uses the stack new command to create a new solution
stackProjectInitializer :: ( MonadIO m
                           , CabalMonad m
                           )
                        => ProjectInitializer (StackProjectInitializerArgs' Text String FilePath) m
stackProjectInitializer = ProjectInitializer $ \arg -> do
    deps <- case traverse simpleParse $ dependencies arg of
        Just deps -> return deps
        Nothing -> throwE $ InvalidOperation "Cannot parse dependencies" ""
    let newBuildInfo = emptyBuildInfo 
                     { hsSourceDirs = primarySrcDir arg : secondarySrcDirs arg
                     , targetBuildDepends = deps
                     }
        newProjectInfo = case arg of
            LibraryProjectArgs{} -> libraryInfo
            _ -> ProjectInfo $ projectName arg
        newCabalProjectInfo = case arg of
            LibraryProjectArgs{} -> LibraryInfo
            ExecutableProjectArgs{ projectName } -> ExecutableInfo $ T.unpack projectName
            TestSuiteProjectArgs{ projectName } -> TestSuiteInfo $ T.unpack projectName
            BenchmarkProjectArgs{ projectName } -> BenchmarkInfo $ T.unpack projectName
        newProject = case arg of
            ExecutableProjectArgs{} -> ExecutableProject (ProjectInfo $ projectName arg)
                $ emptyExecutable 
                { buildInfo = newBuildInfo
                , exeName = T.unpack $ projectName arg
                , modulePath = exeMainPath arg
                }
            LibraryProjectArgs{} -> LibraryProject
                $ emptyLibrary
                { libBuildInfo = newBuildInfo
                }
            TestSuiteProjectArgs{} -> TestSuiteProject (ProjectInfo $ projectName arg)
                $ emptyTestSuite
                { testName = T.unpack $ projectName arg
                , testInterface = case testSuiteArgs arg of
                    StdioTestSuiteArgs path -> TestSuiteExeV10 (Version [1,0] []) path
                    DetailedTestSuiteArgs name -> TestSuiteLibV09 (Version [0,9] []) $ fromString name
                , testBuildInfo = newBuildInfo
                , testEnabled = True
                }
            BenchmarkProjectArgs{} -> BenchmarkProject (ProjectInfo $ projectName arg)
                $ emptyBenchmark
                { benchmarkName = T.unpack $ projectName arg
                , benchmarkInterface = case benchmarkArgs arg of
                    StdioBenchmarkArgs path -> BenchmarkExeV10 (Version [1,0] []) path
                , benchmarkBuildInfo = newBuildInfo
                , benchmarkEnabled = True
                }
    addCabalProject newCabalProjectInfo newProject
    return $ ProjectInitializerSucceeded "" "" newProjectInfo
