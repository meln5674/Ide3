{-# LANGUAGE OverloadedStrings #-}
module Runner.Stack where

import Data.Monoid
import qualified Data.Text as T

import Control.Exception.Base hiding (catch)

import Distribution.Package
import Distribution.PackageDescription hiding (testName)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Exit
import System.Process.Text

import Runner

import CabalMonad

import Ide3.Types

-- | A Runner which uses stack exec to run a solution
stackRunner :: (MonadIO m, MonadMask m, CabalMonad m) => Runner m
stackRunner = MkRunner $ \pji cmdArgs -> do
    cpji <- getCabalProjectInfo pji
    maybeArgs <- case cpji of
        LibraryInfo -> return Nothing
        ExecutableInfo execName -> return $ Just $ ["exec",execName,"--"] ++ cmdArgs
        BenchmarkInfo benchName -> do
            pkgDesc <- getPackageDescription
            let benchArg = unPackageName (pkgName $ package pkgDesc) ++ ':' : benchName
            return $ Just $ ["bench",benchArg,"--benchmark-arguments"] ++ cmdArgs
        TestSuiteInfo testName -> do
            pkgDesc <- getPackageDescription
            let testArg = unPackageName (pkgName $ package pkgDesc) ++ ':' : testName
            return $ Just $ ["test",testArg,"--test-arguments"] ++ cmdArgs
    case maybeArgs of
        Nothing -> throwE $ InvalidOperation "Cannot run a library" ""
        Just args -> ExceptT $ flip catch handleException $ do 
            (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" args ""
            case ec of
                ExitSuccess -> return $ Right $ RunSucceeded out err
                ExitFailure _ -> return $ Right $ RunFailed out err
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
