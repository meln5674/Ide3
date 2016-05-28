{-|
Module      : Initializer
Description : Initializing projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Initializer is an abstract data type which attempts to create a new project
-}
module Initializer
    ( InitializerResult (..)
    , Initializer
    , runInitializer
    , runInitializerWithInput
    , TemplateName
    , StackInitializerArgs (..)
    , noInitializer
    , stackInitializer
    , Args (..)
    ) where

import System.Exit
import System.Process
import System.Directory

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Monad
import Ide3.Digest

-- | Class for types which can be parsed from a list of strings
class Args a where
    getArgsFrom :: [String] -> Either String a

-- | The result of initialization
data InitializerResult
    = InitializerSucceeded String String
    | InitializerFailed String String

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype Initializer a m u = Initializer
    { runInitializerInternal :: a -> ProjectResult m u InitializerResult }

-- | Run an initializer with a list of strings to parse into arguments
runInitializerWithInput :: (ProjectM m, Args a) 
               => Initializer a m u 
               -> [String]
               -> Either String (ProjectResult m u InitializerResult)
runInitializerWithInput initializer = liftM (runInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runInitializer :: (ProjectM m, Args a)
               => Initializer a m u
               -> a
               -> ProjectResult m u InitializerResult
runInitializer = runInitializerInternal

-- | The name of a stack template
type TemplateName = String

-- | The arguments to initialize a stack project, a file path and optional a template name
data StackInitializerArgs = StackInitializerArgs FilePath (Maybe TemplateName)

instance Args StackInitializerArgs where
    getArgsFrom [path] = Right $ StackInitializerArgs path Nothing
    getArgsFrom [template,path] = Right $ StackInitializerArgs path (Just template)
    getArgsFrom [] = Left "new expects at least a path and optionally a template name"
    getArgsFrom _ = Left "new expects only a path and optionally a template name"

-- | An Initializer that represents no initialization capability, and will
-- always result in an error
noInitializer :: Monad m => Initializer a m u
noInitializer = Initializer $ \_ -> throwE $ Unsupported "No initializer specified"

-- | An Initializer that uses the stack new command to create a new project
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
        ExitFailure _ ->  return $ InitializerFailed out err
