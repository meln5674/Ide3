{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ProjectInitializer.Stack.Types where

import Data.String (IsString(..))

import Data.Text (Text)
import qualified Data.Text as T

import Ide3.Types

import CabalMonad

import Args

-- | The arguments to initialize a stack solution, a file path and optional a template name
data StackProjectInitializerArgs = StackProjectInitializerArgs CabalProjectInfo FilePath


data TestSuiteProjectArgs text path
    = StdioTestSuiteArgs path
    | DetailedTestSuiteArgs text

data BenchmarkProjectArgs text path
    = StdioBenchmarkArgs path

data StackProjectInitializerArgs' name text path
    = LibraryProjectArgs 
    { primarySrcDir :: path
    , secondarySrcDirs :: [path]
    , dependencies :: [text]
    }
    | ExecutableProjectArgs
    { projectName :: name
    , primarySrcDir :: path
    , secondarySrcDirs :: [path]
    , dependencies :: [text]
    , exeMainPath :: path
    }
    | TestSuiteProjectArgs
    { projectName :: name
    , primarySrcDir :: path
    , secondarySrcDirs :: [path]
    , dependencies :: [text]
    , testSuiteArgs :: TestSuiteProjectArgs text path
    }
    | BenchmarkProjectArgs
    { projectName :: name
    , primarySrcDir :: path
    , secondarySrcDirs :: [path]
    , dependencies :: [text]
    , benchmarkArgs :: BenchmarkProjectArgs text path
    }

mapTestSuiteArgs :: (a -> a') -> (b -> b') -> TestSuiteProjectArgs a b -> TestSuiteProjectArgs a' b'
mapTestSuiteArgs f g (StdioTestSuiteArgs x) = StdioTestSuiteArgs (g x)
mapTestSuiteArgs f g (DetailedTestSuiteArgs x) = DetailedTestSuiteArgs (f x)


mapBenchmarkArgs :: (a -> a') -> (b -> b') -> BenchmarkProjectArgs a b -> BenchmarkProjectArgs a' b'
mapBenchmarkArgs f g (StdioBenchmarkArgs x) = StdioBenchmarkArgs (g x)


mapStackArgs :: (a -> a')
             -> (b -> b')
             -> (c -> c')
             -> StackProjectInitializerArgs' a b c 
             -> StackProjectInitializerArgs' a' b' c'
mapStackArgs f g h LibraryProjectArgs{..}
    = LibraryProjectArgs
    { primarySrcDir = h primarySrcDir
    , secondarySrcDirs = map h secondarySrcDirs
    , dependencies = map g dependencies
    }
mapStackArgs f g h ExecutableProjectArgs{..}
    = ExecutableProjectArgs
    { projectName = f projectName
    , primarySrcDir = h primarySrcDir
    , secondarySrcDirs = map h secondarySrcDirs
    , dependencies = map g dependencies
    , exeMainPath = h exeMainPath
    }
mapStackArgs f g h TestSuiteProjectArgs{..}
    = TestSuiteProjectArgs
    { projectName = f projectName
    , primarySrcDir = h primarySrcDir
    , secondarySrcDirs = map h secondarySrcDirs
    , dependencies = map g dependencies
    , testSuiteArgs = mapTestSuiteArgs g h testSuiteArgs
    }
mapStackArgs f g h BenchmarkProjectArgs{..}
    = BenchmarkProjectArgs
    { projectName = f projectName
    , primarySrcDir = h primarySrcDir
    , secondarySrcDirs = map h secondarySrcDirs
    , dependencies = map g dependencies
    , benchmarkArgs = mapBenchmarkArgs g h benchmarkArgs
    }

getProjectInfo :: StackProjectInitializerArgs' Text text path -> ProjectInfo
getProjectInfo LibraryProjectArgs{} = libraryInfo
getProjectInfo args = ProjectInfo $ projectName args

instance Args StackProjectInitializerArgs where
    getArgsFrom ["executable",exeName,srcDir]
        = Right $ StackProjectInitializerArgs (ExecutableInfo $ fromString exeName) (fromString srcDir)
    getArgsFrom ["library",srcDir]
        = Right $ StackProjectInitializerArgs LibraryInfo (fromString srcDir)
    getArgsFrom ["test-suite",testName,srcDir]
        = Right $ StackProjectInitializerArgs (TestSuiteInfo testName) (fromString srcDir)
    getArgsFrom ["benchmark",benchName,srcDir]
        = Right $ StackProjectInitializerArgs (BenchmarkInfo benchName) (fromString srcDir)
    getArgsFrom _ = Left $ unlines 
        [ "Usage: add project executable EXENAME SOURCEDIR"
        , "       add project library SOURCEDIR"
        , "       add project test-suite TESTSUITENAME SOURCEDIR"
        , "       add project benchmark BENCHMARKNAME SOURCEDIR"
        ]

instance ( IsString name
         , IsString text
         , IsString path
         ) 
     => Args (StackProjectInitializerArgs' name text path) where
    getArgsFrom ["executable",exeName,srcDir,path]
        = Right $ ExecutableProjectArgs (fromString exeName) (fromString srcDir) [] [] (fromString path)
    getArgsFrom ["library",srcDir]
        = Right $ LibraryProjectArgs (fromString srcDir) [] []
    getArgsFrom ["test-suite",testName,srcDir,"stdio",path]
        = Right $ TestSuiteProjectArgs (fromString testName) (fromString srcDir) [] [] $ StdioTestSuiteArgs (fromString path)
    getArgsFrom ["test-suite",testName,srcDir,"detailed",name]
        = Right $ TestSuiteProjectArgs (fromString testName) (fromString srcDir) [] [] $ DetailedTestSuiteArgs (fromString name)
    getArgsFrom ["benchmark",benchName,srcDir,"stdio",path]
        = Right $ BenchmarkProjectArgs (fromString benchName) (fromString srcDir) [] [] $ StdioBenchmarkArgs (fromString path)
    getArgsFrom _ = Left $ unlines 
        [ "Usage: add project executable EXENAME SOURCEDIR MAINPATH"
        , "       add project library SOURCEDIR"
        , "       add project test-suite TESTSUITENAME SOURCEDIR stdio MAINPATH"
        , "       add project test-suite TESTSUITENAME SOURCEDIR detailed MAINMODULE"
        , "       add project benchmark BENCHMARKNAME SOURCEDIR stdio MAINPATH"
        ]

