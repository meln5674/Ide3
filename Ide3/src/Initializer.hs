{-|
Module      : Initializer
Description : Initializing solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Initializer is an abstract data type which attempts to create a new solution
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
    ) where

import System.Exit
import System.Process
import System.Directory
import System.FilePath

import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Monad
import Ide3.Digest

import Args
import Viewer

-- | The result of initialization
data InitializerResult
    = InitializerSucceeded String String
    | InitializerFailed String String

-- | The initializer abstract type. Use runInitializer or runInitializerWithInput
-- to execute the actions of an initializer
newtype Initializer a m u = Initializer
    { runInitializerInternal :: a -> SolutionResult m u InitializerResult }

-- | Run an initializer with a list of strings to parse into arguments
runInitializerWithInput :: (SolutionM m, Args a) 
               => Initializer a m u 
               -> [String]
               -> Either String (SolutionResult m u InitializerResult)
runInitializerWithInput initializer = liftM (runInitializerInternal initializer) . getArgsFrom

-- | Run an initializer with its arguments
runInitializer :: (SolutionM m, Args a)
               => Initializer a m u
               -> a
               -> SolutionResult m u InitializerResult
runInitializer = runInitializerInternal

-- | The name of a stack template
type TemplateName = String

-- | The arguments to initialize a stack solution, a file path and optional a template name
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

-- | An Initializer that uses the stack new command to create a new solution
stackInitializer :: (MonadIO m, SolutionM m) => Initializer StackInitializerArgs m u
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
            let solutionName = takeBaseName path
                solutionInfo = SolutionInfo solutionName
                projects = flip map dirs $ \d -> (ProjectInfo $ takeBaseName d, d,Nothing)
            digestSolutionM solutionInfo path projects
            return $ InitializerSucceeded out err
            
        ExitFailure _ ->  return $ InitializerFailed out err
