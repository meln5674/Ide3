{-# LANGUAGE OverloadedStrings #-}
module Builder.Stack where

import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import Control.Exception.Base hiding (catch)

import System.FilePath
import System.Directory
import System.Exit
import System.Process.Text

import Distribution.PackageDescription hiding (description)
import Distribution.Version
import Distribution.Package

import Ide3.Types

import CabalMonad

import Builder

import ErrorParser

-- | A builder which uses stack to build a stack solution
stackBuilder :: (MonadIO m, MonadMask m, CabalMonad m) => Builder m
stackBuilder = MkBuilder $ flip catch handleException $ do
    (ec, _, err) <- liftIO $ readProcessWithExitCode "stack" ["build"] ""
    desc <- getPackageDescription
    let pkgString pkg = 
            unPackageName (pkgName $ package pkg) 
            ++ "-" 
            ++ intercalate "." (map show $ versionBranch $ pkgVersion $ package pkg)
        description = pkgString desc
    (_, rootDir, _) <- liftIO $ readProcessWithExitCode "stack" ["path", "--project-root"] ""
    let logPath = init (T.unpack rootDir) </> ".stack-work" </> "logs" </> description <.> "log"
    logExists <- liftIO $ doesFileExist logPath
    --liftIO $ putStrLn logPath
    buildLog <- do
        logContents <- if logExists
            then liftIO $ T.readFile logPath
            else return "" --return "I COULDN'T FIND THE LOG FILE\n"
        return $ err <> logContents 
    errorList <- parseLog buildLog
    case errorList of
        Nothing -> throwE $ InternalError "Could not parse build log" ""
        Just errors -> case ec of
            ExitSuccess -> return $ BuildSucceeded buildLog errors
            ExitFailure _ -> return $ BuildFailed buildLog errors
  where
    handleException e = throwE $ InvalidOperation (show (e :: IOException)) ""
