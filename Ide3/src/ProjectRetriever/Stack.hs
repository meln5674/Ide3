{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module ProjectRetriever.Stack where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text

import Ide3.Types
import Ide3.NewMonad

import Args

import ProjectRetriever

import CabalMonad


import ProjectInitializer.Stack.Types
    
stackProjectRetriever :: ( MonadIO m
                          , PersistenceClass m
                          , CabalMonad m
                          , SolutionMonad m
                          )
                       => ProjectRetriever StackProjectInitializerArgs' m
stackProjectRetriever = ProjectRetriever $ \pji -> do
    p <- lookupCabalProject pji
    case p of
        ExecutableProject (ProjectInfo projectName) exe -> do
            let buildInfo' = buildInfo exe
                (primarySrcDir : secondarySrcDirs) = hsSourceDirs buildInfo'
                dependencies = map display $ targetBuildDepends buildInfo'
                exeMainPath = modulePath exe
            return $ ExecutableProjectArgs { projectName
                                           , primarySrcDir
                                           , secondarySrcDirs
                                           , dependencies
                                           , exeMainPath
                                           }
        LibraryProject lib -> do
            let buildInfo = libBuildInfo lib
                (primarySrcDir : secondarySrcDirs) = hsSourceDirs buildInfo
                dependencies = map display $ targetBuildDepends buildInfo
            return $ LibraryProjectArgs { primarySrcDir
                                        , secondarySrcDirs
                                        , dependencies
                                        }
        TestSuiteProject (ProjectInfo projectName) test -> do
            let buildInfo = testBuildInfo test
                (primarySrcDir : secondarySrcDirs) = hsSourceDirs buildInfo
                dependencies = map display $ targetBuildDepends buildInfo
                testSuiteArgs = case testInterface test of
                    TestSuiteExeV10 _ path -> StdioTestSuiteArgs path
                    TestSuiteLibV09 _ name -> DetailedTestSuiteArgs $ display name
            return $ TestSuiteProjectArgs { projectName
                                          , primarySrcDir
                                          , secondarySrcDirs
                                          , dependencies
                                          , testSuiteArgs
                                          }
        BenchmarkProject (ProjectInfo projectName) bench -> do
            let buildInfo = benchmarkBuildInfo bench
                (primarySrcDir : secondarySrcDirs) = hsSourceDirs buildInfo
                dependencies = map display $ targetBuildDepends buildInfo
                benchmarkArgs = case benchmarkInterface bench of
                    BenchmarkExeV10 _ path -> StdioBenchmarkArgs path
            return $ BenchmarkProjectArgs { projectName
                                          , primarySrcDir
                                          , secondarySrcDirs
                                          , dependencies
                                          , benchmarkArgs
                                          }
