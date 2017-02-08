{-# LANGUAGE OverloadedStrings #-}
module Builder.Stack where

import Data.Attoparsec.Text as P

import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.List

import Data.Conduit ( (.|) )
import qualified Data.Conduit as C
import qualified Data.Conduit.Process as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Text as C
import qualified Data.Conduit.Binary as C hiding (lines)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch
import Control.Monad.Trans.Resource

import Control.Exception.Base hiding (catch)

import System.FilePath
import System.Directory
import System.Exit
import System.Process.Text
import System.Process (createProcess, shell, waitForProcess)

import Distribution.PackageDescription hiding (description)
import Distribution.Version
import Distribution.Package

import Ide3.Types

import CabalMonad

import Builder

import ErrorParser

-- | A builder which uses stack to build a stack solution
stackBuilder :: (MonadResource m, MonadMask m, CabalMonad m) => Builder m
stackBuilder = MkBuilder $ \onLine onError -> flip catch handleException $ do
    let lineWatcher = do
            let loop = do
                    maybeLine <- C.await
                    case maybeLine of
                        Just nextLine -> do
                            lift $ onLine nextLine
                            C.yield nextLine
                            loop
                        Nothing -> return ()
            loop
        errorWatcher = do
            let loop = do
                    maybeError <- C.await
                    case maybeError of
                        Just err -> do
                            lift $ onError err
                            loop
                        _ -> return ()
            loop
        errorStreamer = do
            let loop = do
                    maybeErrors <- C.await
                    case maybeErrors of
                        Just (Right (_, errors)) -> do
                            mapM_ C.yield errors
                            loop
                        Nothing -> return ()
            loop
        stderrStream = C.decode C.utf8
            .| C.lines 
            .| lineWatcher 
            .| C.conduitParserEither ErrorParser.project
            .| errorStreamer
            .| errorWatcher
        
    (_, _, (Just stderr), pHandle) <- liftIO $ createProcess (shell "stack build") 
    
    lift $ C.runConduit $ C.sourceHandle stderr .| stderrStream
    
    ec <- liftIO $ waitForProcess pHandle
    
    desc <- getPackageDescription
    let pkgString pkg = 
            (unPackageName $ pkgName $ package pkg) 
            ++ "-" 
            ++ (intercalate "." $ map show $ versionBranch $ pkgVersion $ package pkg)
        description = pkgString desc
    (_, rootDir, _) <- liftIO $ readProcessWithExitCode "stack" ["path", "--project-root"] ""
    let logPath = init (T.unpack rootDir) </> ".stack-work" </> "logs" </> description <.> "log"
    logExists <- liftIO $ doesFileExist logPath
    
    lift $ when logExists $ do
        C.runConduit $ C.sourceFile logPath .| stderrStream
    
    {-
    --liftIO $ putStrLn logPath
    buildLog <- do
        logContents <- if logExists
            then liftIO $ T.readFile logPath
            else return "" --return "I COULDN'T FIND THE LOG FILE\n"
        return $ err <> logContents 
    -}
    {-
    errorList <- parseLog buildLog
    case errorList of
        Nothing -> do
            throwE $ InternalError "Could not parse build log" ""
        Just errors -> case ec of
            ExitSuccess -> return $ BuildSucceeded buildLog errors
            ExitFailure _ -> return $ BuildFailed buildLog errors
    -}
    case ec of
        ExitSuccess -> return BuildSucceeded
        ExitFailure _ -> return BuildFailed
  where
    handleException e = throwE $ InvalidOperation (show (e :: IOException)) ""
