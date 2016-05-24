module Initializer
    ( InitializerResult (..)
    , Initializer
    , runInitializer
    , TemplateName
    , StackInitializerArgs (..)
    , stackInitializer
    , Args (..)
    ) where

import System.Exit
import System.Process
import System.Directory
import System.FilePath

import Data.List

import Control.Monad
import Control.Monad.Trans

import Ide3.Monad
import Ide3.Digest

import CmdParser

class Args a where
    getArgsFrom :: [String] -> Either String a

data InitializerResult
    = InitializerSucceeded String String
    | InitializerFailed String String

newtype Initializer a m u = Initializer
    { runInitializerInternal :: a -> ProjectResult m u InitializerResult }

runInitializer :: (ProjectM m, Args a) 
               => Initializer a m u 
               -> String
               -> Either String (ProjectResult m u InitializerResult)
runInitializer initializer = liftM (runInitializerInternal initializer) . getArgsFrom <=< parseArityN 

type TemplateName = String

data StackInitializerArgs = StackInitializerArgs FilePath (Maybe TemplateName)

instance Args StackInitializerArgs where
    getArgsFrom [path] = Right $ StackInitializerArgs path Nothing
    getArgsFrom [template,path] = Right $ StackInitializerArgs path (Just template)
    getArgsFrom [] = Left $ "new expects at least a path and optionally a template name"
    getArgsFrom _ = Left $ "new expects only a path and optionally a template name"

stackInitializer :: (MonadIO m, ProjectM m) => Initializer StackInitializerArgs m u
stackInitializer = Initializer $ \(StackInitializerArgs path template) -> do
    let args = case template of
            Nothing -> ["new", path]
            Just template_name -> ["new", path, template_name]
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" args ""
    case ec of
        ExitSuccess -> do
            files <- liftM (delete "." . delete "..") $ liftIO $ getDirectoryContents path
            liftIO $ setCurrentDirectory path
            dirs <- liftIO $ filterM doesDirectoryExist files
            mapM_ digestProject dirs
            return $ InitializerSucceeded out err
        ExitFailure _ -> do
            return $ InitializerFailed out err
