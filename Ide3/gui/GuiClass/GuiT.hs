{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GuiClass.GuiT where

import qualified Data.Text as T

import System.Directory
import System.FilePath

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import ViewerMonad

import Ide3.Types
import Ide3.Utils

--import Graphics.UI.Gtk

import EnvironmentMonad

import Initializer.Stack
import ProjectInitializer.Stack()
import ProjectInitializer.Stack.Types

import GuiClass

import Dialogs.Class

import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.NewProjectDialog as NewProjectDialog
import Dialogs.NewProjectDialog (ProjectType(..), TestSuiteType(..), DialogMode(..))

import GuiT


instance ( MonadIO m
         , ProjectArgType m' ~ StackProjectInitializerArgs'
         ) => ProjectInitializerClass (GuiT m' p m) where
    type ClassProjectInitializerMonad (GuiT m' p m) = m'
    setupProjectCreator Nothing
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> do
            NewProjectDialog.setVisible dialog True
            NewProjectDialog.setDialogMode dialog CreateProject
            NewProjectDialog.resetFields dialog
    setupProjectCreator (Just arg)
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> do
            NewProjectDialog.setVisible dialog True
            NewProjectDialog.resetFields dialog
            NewProjectDialog.setDialogMode dialog 
                $ EditProject 
                $ T.pack 
                $ (let ProjectInfo projectName = getProjectInfo arg in projectName)
            NewProjectDialog.setPrimarySrcDir dialog 
                $ T.pack 
                $ primarySrcDir arg
            NewProjectDialog.setSecondarySrcDirs dialog 
                $ T.intercalate ", " 
                $ map T.pack 
                $ secondarySrcDirs arg
            NewProjectDialog.setDependencies dialog 
                $ T.intercalate ", " 
                $ map T.pack 
                $ dependencies arg
            case arg of
                ExecutableProjectArgs{ projectName, exeMainPath } -> do
                    NewProjectDialog.setProjectType dialog Executable
                    NewProjectDialog.setExecutableProjectName dialog $ T.pack projectName
                    NewProjectDialog.setExecutableMainModule dialog $ T.pack exeMainPath
                LibraryProjectArgs{} -> NewProjectDialog.setProjectType dialog Library
                TestSuiteProjectArgs{ projectName, testSuiteArgs } -> do
                    NewProjectDialog.setProjectType dialog TestSuite
                    NewProjectDialog.setTestProjectName dialog $ T.pack projectName
                    case testSuiteArgs of
                        StdioTestSuiteArgs path -> do
                            NewProjectDialog.setTestProjectType dialog ExitCode
                            NewProjectDialog.setTestMainModule dialog $ T.pack path
                        DetailedTestSuiteArgs name -> do
                            NewProjectDialog.setTestProjectType dialog Detailed
                            NewProjectDialog.setTestTestModule dialog $ T.pack name
                BenchmarkProjectArgs{ projectName, benchmarkArgs } -> do
                    NewProjectDialog.setProjectType dialog Benchmark
                    NewProjectDialog.setBenchmarkProjectName dialog $ T.pack projectName
                    case benchmarkArgs of
                        StdioBenchmarkArgs path -> do
                            NewProjectDialog.setBenchmarkMainModule dialog $ T.pack path
            
    getProjectCreatorArg
        = liftDialogs 
        $ withNewProjectDialogM
        $ \dialog -> do
            let fromCommaList = map (T.unpack . T.strip) . T.splitOn ","
            primarySrcDir <- liftM T.unpack $ NewProjectDialog.getPrimarySrcDir dialog
            secondarySrcDirs <- liftM fromCommaList $ NewProjectDialog.getSecondarySrcDirs dialog
            dependencies <- liftM fromCommaList $ NewProjectDialog.getDependencies dialog
            projectType <- NewProjectDialog.getProjectType dialog
            case projectType of
                Executable -> do
                    projectName <- liftM T.unpack $ NewProjectDialog.getExecutableProjectName dialog
                    exeMainPath <- liftM T.unpack $ NewProjectDialog.getExecutableMainModule dialog
                    return $ Right $ ExecutableProjectArgs 
                        { primarySrcDir
                        , secondarySrcDirs
                        , projectName
                        , exeMainPath
                        , dependencies
                        }
                Library -> do
                    return $ Right $ LibraryProjectArgs
                        { primarySrcDir
                        , secondarySrcDirs
                        , dependencies
                        }
                TestSuite -> do
                    projectName <- liftM T.unpack $ NewProjectDialog.getTestProjectName dialog
                    testType <- NewProjectDialog.getTestProjectType dialog
                    testSuiteArgs <- case testType of
                        ExitCode -> liftM (StdioTestSuiteArgs . T.unpack) 
                            $ NewProjectDialog.getTestMainModule dialog
                        Detailed -> liftM (DetailedTestSuiteArgs . T.unpack)
                            $ NewProjectDialog.getTestTestModule dialog
                    return $ Right $ TestSuiteProjectArgs
                        { projectName
                        , primarySrcDir
                        , secondarySrcDirs
                        , testSuiteArgs
                        , dependencies
                        }
                Benchmark -> do
                    projectName <- liftM T.unpack $ NewProjectDialog.getBenchmarkProjectName dialog
                    benchmarkArgs <- liftM (StdioBenchmarkArgs . T.unpack)
                        $ NewProjectDialog.getBenchmarkMainModule dialog
                    return $ Right $ BenchmarkProjectArgs 
                        { primarySrcDir
                        , secondarySrcDirs
                        , projectName
                        , benchmarkArgs
                        , dependencies
                        }
            
        
        
    finalizeProjectCreator 
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> NewProjectDialog.setVisible dialog False



getSolutionCreatorArg' :: forall m' p m u
                        . ( MonadIO m
                          , ViewerMonad m
                          , m ~ m'
                          )
                       => GuiT m' p m (Either (SolutionError u) StackInitializerArgs)
getSolutionCreatorArg'
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> do
            maybeProjectRoot <- NewSolutionDialog.getSelectedFolder dialog
            projectName <- liftM T.unpack $ NewSolutionDialog.getSolutionName dialog
            templateName <- liftM (fmap T.unpack) $ NewSolutionDialog.getTemplateName dialog
            runExceptT $ case maybeProjectRoot of
                Nothing -> throwE $ InvalidOperation "Please choose a directory" ""
                Just projectRoot -> do
                    wrapIOError $ setCurrentDirectory $ projectRoot
                    --bounce $ setDirectoryToOpen $ projectRoot </> projectName
                    return $ StackInitializerArgs projectName templateName
            


instance ( MonadIO m
         , ViewerMonad m
         , ArgType m' ~ StackInitializerArgs
         , m ~ m'
         ) => SolutionInitializerClass (GuiT m' p m) where
    type ClassSolutionInitializerMonad (GuiT m' p m) = m'
    setupSolutionCreator
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> NewSolutionDialog.setVisible dialog True
    getSolutionCreatorArg = getSolutionCreatorArg'
        
        
    finalizeSolutionCreator 
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> NewSolutionDialog.setVisible dialog False



