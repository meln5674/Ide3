{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module ProjectEditor.Stack where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text

import Ide3.Types
import Ide3.NewMonad

import Args

import ProjectEditor

import CabalMonad


import ProjectInitializer.Stack.Types

-- | An Editor that uses the stack new command to create a new solution
stackProjectEditor :: ( MonadIO m
                           , PersistenceClass m
                           , CabalMonad m
                           , SolutionClass m
                           )
                        => ProjectEditor StackProjectInitializerArgs' m
stackProjectEditor = ProjectEditor $ \pji arg -> do
    oldCabalProjectInfo <- getCabalProjectInfo pji
    p <- getCabalProject oldCabalProjectInfo
    deps <- case traverse simpleParse $ dependencies arg of
        Just deps -> return deps
        Nothing -> throwE $ InvalidOperation "Cannot parse dependencies" ""
    let oldBuildInfo = case p of
            ExecutableProject _ Executable{ buildInfo } -> buildInfo
            LibraryProject Library{ libBuildInfo } -> libBuildInfo
            TestSuiteProject _ TestSuite{ testBuildInfo } -> testBuildInfo
            BenchmarkProject _ Benchmark{ benchmarkBuildInfo } -> benchmarkBuildInfo
        newBuildInfo = oldBuildInfo
                     { hsSourceDirs = primarySrcDir arg : secondarySrcDirs arg
                     , targetBuildDepends = deps
                     }
        newProjectInfo = case arg of
            LibraryProjectArgs{} -> libraryInfo
            _ -> ProjectInfo $ projectName arg
        newCabalProjectInfo = case arg of
            LibraryProjectArgs{} -> LibraryInfo
            ExecutableProjectArgs{} -> ExecutableInfo $ projectName arg
            TestSuiteProjectArgs{} -> TestSuiteInfo $ projectName arg
            BenchmarkProjectArgs{} -> BenchmarkInfo $ projectName arg
        newProject = case arg of
            ExecutableProjectArgs{} -> ExecutableProject (ProjectInfo $ projectName arg)
                $ (case p of { ExecutableProject _ exe -> exe; _ -> emptyExecutable })
                { buildInfo = newBuildInfo
                , exeName = projectName arg
                , modulePath = exeMainPath arg
                }
            LibraryProjectArgs{} -> LibraryProject
                $ (case p of { LibraryProject lib -> lib; _ -> emptyLibrary })
                { libBuildInfo = newBuildInfo
                }
            TestSuiteProjectArgs{} -> TestSuiteProject (ProjectInfo $ projectName arg)
                $ (case p of { TestSuiteProject _ test -> test; _ -> emptyTestSuite{ testEnabled = True}})
                { testName = projectName arg
                , testInterface = case testSuiteArgs arg of
                    StdioTestSuiteArgs path -> TestSuiteExeV10 (Version [1,0] []) path
                    DetailedTestSuiteArgs name -> TestSuiteLibV09 (Version [0,9] []) $ fromString name
                , testBuildInfo = newBuildInfo
                }
            BenchmarkProjectArgs{} -> BenchmarkProject (ProjectInfo $ projectName arg)
                $ (case p of { BenchmarkProject _ bench -> bench; _ -> emptyBenchmark{ benchmarkEnabled = True}})
                { benchmarkName = projectName arg
                , benchmarkInterface = case benchmarkArgs arg of
                    StdioBenchmarkArgs path -> BenchmarkExeV10 (Version [1,0] []) path
                , benchmarkBuildInfo = newBuildInfo
                , benchmarkEnabled = True
                }
    removeCabalProject oldCabalProjectInfo
    addCabalProject newCabalProjectInfo newProject
    return $ ProjectEditorSucceeded "" "" newProjectInfo
