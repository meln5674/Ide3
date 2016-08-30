module Builder.Stack where

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import Control.Exception.Base hiding (catch)

import System.FilePath
import System.Directory
import System.Exit
import System.Process

import Distribution.PackageDescription
import Distribution.Version
import Distribution.Package

import Ide3.NewMonad
import Ide3.Types
import Ide3.Utils

import CabalMonad

import Builder

import ErrorParser
import ErrorParser.Types

-- | A builder which uses stack to build a stack solution
stackBuilder :: (MonadIO m, MonadMask m, CabalMonad m) => Builder m
stackBuilder = MkBuilder $ flip catch handleException $ do
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" ["build"] ""
    desc <- getPackageDescription
    let pkgString pkg = 
            (unPackageName $ pkgName $ package pkg) 
            ++ "-" 
            ++ (intercalate "." $ map show $ versionBranch $ pkgVersion $ package pkg)
        description = pkgString desc
    rootDir <- liftIO $ readProcess "stack" ["path", "--project-root"] ""
    let logPath = init rootDir </> ".stack-work" </> "logs" </> description <.> "log"
    logExists <- liftIO $ doesFileExist logPath
    --liftIO $ putStrLn logPath
    buildLog <- do
        logContents <- if logExists
            then liftIO $ readFile logPath
            else return "I COULDN'T FIND THE LOG FILE\n"
        return $ err ++ logContents 
    let errorList = parseLog $ buildLog
    case errorList of
        Nothing -> do
            throwE $ InternalError "Could not parse build log" ""
        Just errors -> case ec of
            ExitSuccess -> return $ BuildSucceeded buildLog errors
            ExitFailure _ -> return $ BuildFailed buildLog errors
  where
    handleException e = throwE $ InvalidOperation (show (e :: IOException)) ""
