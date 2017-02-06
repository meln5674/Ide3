{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module ProjectRemover.Stack where

import Data.Text (Text)
import qualified Data.Text as T

import System.IO.Error

import Control.Monad.Trans
import Control.Monad.Trans.Except

import System.Directory

import Distribution.PackageDescription
import Distribution.Version
import Distribution.ModuleName
import Distribution.Text hiding (Text)

import Ide3.Types

import ProjectRemover

import CabalMonad

import ProjectInitializer.Stack.Types

stackProjectRemover :: ( MonadIO m
                       , CabalMonad m
                       )
                    => ProjectRemover (StackProjectInitializerArgs' Text String FilePath) m
stackProjectRemover = ProjectRemover $ \arg -> do
    let cabalProjectInfo = case arg of
            LibraryProjectArgs{} -> LibraryInfo
            ExecutableProjectArgs{} -> ExecutableInfo $ T.unpack $ projectName arg
            TestSuiteProjectArgs{} -> TestSuiteInfo $ T.unpack $ projectName arg
            BenchmarkProjectArgs{} -> BenchmarkInfo $ T.unpack $ projectName arg
    removeCabalProject cabalProjectInfo
    result <- liftIO $ tryIOError $ removeDirectoryRecursive $ primarySrcDir arg
    return $ case result of
        Left err -> ProjectRemoverFailed "" (show err)
        Right _ -> ProjectRemoverSucceeded "" ""
