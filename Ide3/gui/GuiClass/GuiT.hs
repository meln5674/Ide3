{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GuiClass.GuiT where

import Data.Text (Text)
import qualified Data.Text as T

import System.Directory

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types
import Ide3.Utils

--import Graphics.UI.Gtk

import EnvironmentMonad

import Initializer.Stack
import ProjectInitializer.Stack()
import ProjectInitializer.Stack.Types

import GuiClass

import Dialogs.Class

import qualified Dialogs.MainWindow as MainWindow
import qualified Dialogs.NewSolutionDialog as NewSolutionDialog
import qualified Dialogs.NewProjectDialog as NewProjectDialog
import Dialogs.NewProjectDialog (ProjectType(..), TestSuiteType(..), DialogMode(..))

import GuiT


instance ( MonadIO m
         , ProjectArgType m' ~ StackProjectInitializerArgs' Text String FilePath
         ) => ProjectInitializerClass (GuiT m' p m) where
    type ClassProjectInitializerMonad (GuiT m' p m) = m'
    setupProjectCreator Nothing
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> do
            NewProjectDialog.setVisible dialog True
            NewProjectDialog.setDialogMode dialog CreateProject
            NewProjectDialog.resetFields dialog
    setupProjectCreator (Just arg')
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> do
            NewProjectDialog.setVisible dialog True
            NewProjectDialog.resetFields dialog
            NewProjectDialog.setDialogMode dialog 
                $ EditProject 
                (let ProjectInfo projectName = getProjectInfo arg in projectName)
            NewProjectDialog.setPrimarySrcDir dialog 
                $ primarySrcDir arg
            NewProjectDialog.setSecondarySrcDirs dialog 
                $ T.intercalate ", " 
                $ secondarySrcDirs arg
            NewProjectDialog.setDependencies dialog 
                $ T.intercalate ", " 
                $ dependencies arg
            case arg of
                ExecutableProjectArgs{ projectName, exeMainPath } -> do
                    NewProjectDialog.setProjectType dialog Executable
                    NewProjectDialog.setExecutableProjectName dialog projectName
                    NewProjectDialog.setExecutableMainModule dialog exeMainPath
                LibraryProjectArgs{} -> NewProjectDialog.setProjectType dialog Library
                TestSuiteProjectArgs{ projectName, testSuiteArgs } -> do
                    NewProjectDialog.setProjectType dialog TestSuite
                    NewProjectDialog.setTestProjectName dialog projectName
                    case testSuiteArgs of
                        StdioTestSuiteArgs path -> do
                            NewProjectDialog.setTestProjectType dialog ExitCode
                            NewProjectDialog.setTestMainModule dialog path
                        DetailedTestSuiteArgs name -> do
                            NewProjectDialog.setTestProjectType dialog Detailed
                            NewProjectDialog.setTestTestModule dialog name
                BenchmarkProjectArgs{ projectName, benchmarkArgs } -> do
                    NewProjectDialog.setProjectType dialog Benchmark
                    NewProjectDialog.setBenchmarkProjectName dialog projectName
                    case benchmarkArgs of
                        StdioBenchmarkArgs path -> 
                            NewProjectDialog.setBenchmarkMainModule dialog path
      where
        arg = mapStackArgs id T.pack T.pack arg'
            
    getProjectCreatorArg
        = liftDialogs 
        $ withNewProjectDialogM
        $ \dialog -> do
            let fromCommaList = map T.strip . T.splitOn ","
            primarySrcDir <- NewProjectDialog.getPrimarySrcDir dialog
            secondarySrcDirs <- fromCommaList <$> NewProjectDialog.getSecondarySrcDirs dialog
            dependencies <- fromCommaList <$> NewProjectDialog.getDependencies dialog
            projectType <- NewProjectDialog.getProjectType dialog
            arg' <- case projectType of
                Executable -> do
                    projectName <- NewProjectDialog.getExecutableProjectName dialog
                    exeMainPath <- NewProjectDialog.getExecutableMainModule dialog
                    return $ Right ExecutableProjectArgs 
                        { primarySrcDir
                        , secondarySrcDirs
                        , projectName
                        , exeMainPath
                        , dependencies
                        }
                Library -> 
                    return $ Right LibraryProjectArgs
                        { primarySrcDir
                        , secondarySrcDirs
                        , dependencies
                        }
                TestSuite -> do
                    projectName <- NewProjectDialog.getTestProjectName dialog
                    testType <- NewProjectDialog.getTestProjectType dialog
                    testSuiteArgs <- case testType of
                        ExitCode -> StdioTestSuiteArgs
                            <$> NewProjectDialog.getTestMainModule dialog
                        Detailed -> DetailedTestSuiteArgs
                            <$> NewProjectDialog.getTestTestModule dialog
                    return $ Right TestSuiteProjectArgs
                        { projectName
                        , primarySrcDir
                        , secondarySrcDirs
                        , testSuiteArgs
                        , dependencies
                        }
                Benchmark -> do
                    projectName <- NewProjectDialog.getBenchmarkProjectName dialog
                    benchmarkArgs <- StdioBenchmarkArgs
                        <$> NewProjectDialog.getBenchmarkMainModule dialog
                    return $ Right BenchmarkProjectArgs 
                        { primarySrcDir
                        , secondarySrcDirs
                        , projectName
                        , benchmarkArgs
                        , dependencies
                        }
            return $ fmap (mapStackArgs id T.unpack T.unpack) arg'
        
        
    finalizeProjectCreator 
        = liftDialogs
        $ withNewProjectDialogM
        $ \dialog -> NewProjectDialog.setVisible dialog False



getSolutionCreatorArg' :: forall m' p m u
                        . ( MonadIO m
                          , m ~ m'
                          )
                       => GuiT m' p m (Either (SolutionError u) StackInitializerArgs)
getSolutionCreatorArg'
        = liftDialogs
        $ withNewSolutionDialogM
        $ \dialog -> do
            maybeProjectRoot <- NewSolutionDialog.getSelectedFolder dialog
            projectName <- NewSolutionDialog.getSolutionName dialog
            templateName <- NewSolutionDialog.getTemplateName dialog
            runExceptT $ case maybeProjectRoot of
                Nothing -> throwE $ InvalidOperation "Please choose a directory" ""
                Just projectRoot -> do
                    wrapIOError $ setCurrentDirectory projectRoot
                    --bounce $ setDirectoryToOpen $ projectRoot </> projectName
                    return $ StackInitializerArgs (T.unpack projectName) (fmap T.unpack templateName)
            


instance ( MonadIO m
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


instance ( MonadIO m ) => EditorControlClass (GuiT m' p m) where
    setEditorEnabled enabled = withMainWindowM $ lift . flip MainWindow.setDeclViewEnabled enabled

instance ( MonadIO m ) => BuildControlClass (GuiT m' p m) where
    setBuildEnabled enabled = withMainWindowM $ lift . flip MainWindow.setBuildButtonEnabled enabled
