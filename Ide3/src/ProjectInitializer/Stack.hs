{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module ProjectInitializer.Stack where

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text

import Ide3.Types
import Ide3.NewMonad

import Args

import ProjectInitializer

import CabalMonad


-- | The arguments to initialize a stack solution, a file path and optional a template name
data StackProjectInitializerArgs = StackProjectInitializerArgs CabalProjectInfo FilePath


data TestSuiteProjectArgs
    = StdioTestSuiteArgs FilePath
    | DetailedTestSuiteArgs String

data BenchmarkProjectArgs
    = StdioBenchmarkArgs FilePath

data StackProjectInitializerArgs'
    = LibraryProjectArgs 
    { primarySrcDir :: FilePath
    , secondarySrcDirs :: [FilePath]
    , dependencies :: [String]
    }
    | ExecutableProjectArgs
    { projectName :: String
    , primarySrcDir :: FilePath
    , secondarySrcDirs :: [FilePath]
    , dependencies :: [String]
    , exeMainPath :: FilePath
    }
    | TestSuiteProjectArgs
    { projectName :: String
    , primarySrcDir :: FilePath
    , secondarySrcDirs :: [FilePath]
    , dependencies :: [String]
    , testSuiteArgs :: TestSuiteProjectArgs
    }
    | BenchmarkProjectArgs
    { projectName :: String
    , primarySrcDir :: FilePath
    , secondarySrcDirs :: [FilePath]
    , dependencies :: [String]
    , benchmarkArgs :: BenchmarkProjectArgs
    }

instance Args StackProjectInitializerArgs where
    getArgsFrom ["executable",exeName,srcDir]
        = Right $ StackProjectInitializerArgs (ExecutableInfo exeName) srcDir
    getArgsFrom ["library",srcDir]
        = Right $ StackProjectInitializerArgs LibraryInfo srcDir
    getArgsFrom ["test-suite",testName,srcDir]
        = Right $ StackProjectInitializerArgs (TestSuiteInfo testName) srcDir
    getArgsFrom ["benchmark",benchName,srcDir]
        = Right $ StackProjectInitializerArgs (BenchmarkInfo benchName) srcDir
    getArgsFrom _ = Left $ unlines 
        [ "Usage: add project executable EXENAME SOURCEDIR"
        , "       add project library SOURCEDIR"
        , "       add project test-suite TESTSUITENAME SOURCEDIR"
        , "       add project benchmark BENCHMARKNAME SOURCEDIR"
        ]

instance Args StackProjectInitializerArgs' where
    getArgsFrom ["executable",exeName,srcDir,path]
        = Right $ ExecutableProjectArgs exeName srcDir [] [] path
    getArgsFrom ["library",srcDir]
        = Right $ LibraryProjectArgs srcDir [] []
    getArgsFrom ["test-suite",testName,srcDir,"stdio",path]
        = Right $ TestSuiteProjectArgs testName srcDir [] [] $ StdioTestSuiteArgs path
    getArgsFrom ["test-suite",testName,srcDir,"detailed",name]
        = Right $ TestSuiteProjectArgs testName srcDir [] [] $ DetailedTestSuiteArgs name
    getArgsFrom ["benchmark",benchName,srcDir,"stdio",path]
        = Right $ BenchmarkProjectArgs benchName srcDir [] [] $ StdioBenchmarkArgs path
    getArgsFrom _ = Left $ unlines 
        [ "Usage: add project executable EXENAME SOURCEDIR MAINPATH"
        , "       add project library SOURCEDIR"
        , "       add project test-suite TESTSUITENAME SOURCEDIR stdio MAINPATH"
        , "       add project test-suite TESTSUITENAME SOURCEDIR detailed MAINMODULE"
        , "       add project benchmark BENCHMARKNAME SOURCEDIR stdio MAINPATH"
        ]

    

-- | An Initializer that uses the stack new command to create a new solution
stackProjectInitializer :: ( MonadIO m
                           , PersistenceClass m
                           , CabalMonad m
                           , SolutionClass m
                           )
                        => ProjectInitializer StackProjectInitializerArgs' m
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
            ExecutableProjectArgs{} -> ExecutableInfo $ projectName arg
            TestSuiteProjectArgs{} -> TestSuiteInfo $ projectName arg
            BenchmarkProjectArgs{} -> BenchmarkInfo $ projectName arg
        newProject = case arg of
            ExecutableProjectArgs{} -> ExecutableProject (ProjectInfo $ projectName arg)
                $ emptyExecutable 
                { buildInfo = newBuildInfo
                , exeName = projectName arg
                , modulePath = exeMainPath arg
                }
            LibraryProjectArgs{} -> LibraryProject
                $ emptyLibrary
                { libBuildInfo = newBuildInfo
                }
            TestSuiteProjectArgs{} -> TestSuiteProject (ProjectInfo $ projectName arg)
                $ emptyTestSuite
                { testName = projectName arg
                , testInterface = case testSuiteArgs arg of
                    StdioTestSuiteArgs path -> TestSuiteExeV10 (Version [1,0] []) path
                    DetailedTestSuiteArgs name -> TestSuiteLibV09 (Version [0,9] []) $ fromString name
                , testBuildInfo = newBuildInfo
                , testEnabled = True
                }
            BenchmarkProjectArgs{} -> BenchmarkProject (ProjectInfo $ projectName arg)
                $ emptyBenchmark
                { benchmarkName = projectName arg
                , benchmarkInterface = case benchmarkArgs arg of
                    StdioBenchmarkArgs path -> BenchmarkExeV10 (Version [1,0] []) path
                , benchmarkBuildInfo = newBuildInfo
                , benchmarkEnabled = True
                }
    addCabalProject newCabalProjectInfo newProject
    return $ ProjectInitializerSucceeded "" "" newProjectInfo
{-
$ \(StackProjectInitializerArgs arg srcDir) -> do
    let newBuildInfo = emptyBuildInfo
                     { hsSourceDirs = [srcDir]
                     }
        newProjectInfo = case arg of
                LibraryInfo -> libraryInfo
                ExecutableInfo exeName -> ProjectInfo exeName
                TestSuiteInfo testName -> ProjectInfo testName
                BenchmarkInfo benchName -> ProjectInfo benchName
        newProject = case arg of
                ExecutableInfo exeName -> ExecutableProject newProjectInfo
                    $ emptyExecutable 
                    { buildInfo = newBuildInfo
                    , exeName = exeName
                    , modulePath = "Main.hs"
                    }
                LibraryInfo -> LibraryProject 
                    $ emptyLibrary
                    { libBuildInfo = newBuildInfo 
                    }
                TestSuiteInfo testName -> TestSuiteProject newProjectInfo
                    $ emptyTestSuite
                    { testName = testName
                    , testInterface = TestSuiteExeV10 (Version [1,0] []) "Main.hs"
                    , testBuildInfo = newBuildInfo
                    , testEnabled = True
                    }
                BenchmarkInfo benchName -> BenchmarkProject newProjectInfo
                    $ emptyBenchmark
                    { benchmarkName = benchName
                    , benchmarkInterface = BenchmarkExeV10 (Version [1,0] []) "Main.hs"
                    , benchmarkBuildInfo = newBuildInfo
                    , benchmarkEnabled = True
                    }
    addCabalProject arg newProject
    finalize
    return $ ProjectInitializerSucceeded "" ""
-}
