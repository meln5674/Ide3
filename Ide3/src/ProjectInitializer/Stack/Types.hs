module ProjectInitializer.Stack.Types where

import Ide3.Types

import CabalMonad

import Args

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

getProjectInfo :: StackProjectInitializerArgs' -> ProjectInfo
getProjectInfo LibraryProjectArgs{} = libraryInfo
getProjectInfo args = ProjectInfo $ projectName args

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

