module ProjectInitializer.Cabal where

import Control.Monad.Trans

import Distribution.PackageDescription
import Distribution.Version

import Ide3.Monad

import Args

import ProjectInitializer

import CabalMonad


-- | The arguments to initialize a stack solution, a file path and optional a template name
data StackProjectInitializerArgs = StackProjectInitializerArgs CabalProjectInfo FilePath

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

-- | An Initializer that uses the stack new command to create a new solution
stackProjectInitializer :: (MonadIO m, SolutionM m, CabalMonad m u) 
                         => ProjectInitializer StackProjectInitializerArgs m u
stackProjectInitializer = ProjectInitializer $ \(StackProjectInitializerArgs arg srcDir) -> do
    let newBuildInfo = emptyBuildInfo
                     { hsSourceDirs = [srcDir]
                     }
        newProject = case arg of
                ExecutableInfo exeName -> ExecutableProject 
                    $ emptyExecutable 
                    { buildInfo = newBuildInfo
                    , exeName = exeName
                    , modulePath = "Main.hs"
                    }
                LibraryInfo -> LibraryProject 
                    $ emptyLibrary
                    { libBuildInfo = newBuildInfo 
                    }
                TestSuiteInfo testName -> TestSuiteProject
                    $ emptyTestSuite
                    { testName = testName
                    , testInterface = TestSuiteExeV10 (Version [1,0] []) "Main.hs"
                    , testBuildInfo = newBuildInfo
                    , testEnabled = True
                    }
                BenchmarkInfo benchName -> BenchmarkProject
                    $ emptyBenchmark
                    { benchmarkName = benchName
                    , benchmarkInterface = BenchmarkExeV10 (Version [1,0] []) "Main.hs"
                    , benchmarkBuildInfo = newBuildInfo
                    , benchmarkEnabled = True
                    }
    addCabalProject arg newProject
    finalize
    return $ ProjectInitializerSucceeded "" ""
