{-|
Module      : Runner
Description : Running projects
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Runner is an abstract data type which attempts to run a project
-}
module Runner
    ( Runner
    , RunnerResult (..)
    , runRunner
    , stackRunner
    , noRunner
    )
    where

import Control.Exception.Base hiding (catch)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Exit
import System.Process

import Ide3.Monad
import Ide3.Types

-- | Result of running a project
data RunnerResult
    = RunFailed String String
    | RunSucceeded String String

-- | The Runner abstract type. Use runRunner to execute the actions of a runner.
newtype Runner m u = MkRunner { runRunnerInternal :: ProjectResult m u RunnerResult }

-- | Execute the actions of a runner
runRunner :: Runner m u -> ProjectResult m u RunnerResult
runRunner = runRunnerInternal

-- | A Runner that represents no ability to run, and will always result in an error
noRunner :: Monad m => Runner m u
noRunner = MkRunner $ throwE $ Unsupported "No runner specified"

-- | A Runner which uses stack exec to run a project
stackRunner :: (MonadIO m, MonadMask m) => Runner m u
stackRunner = MkRunner $ ExceptT $ flip catch handleException $ do
    (_,pkgName,_) <- liftIO $ readProcessWithExitCode "stack" ["ide","packages"] ""
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" ["exec",init pkgName] ""
    case ec of
        ExitSuccess -> return $ Right $ RunSucceeded out err
        ExitFailure _ -> return $ Right $ RunFailed out err
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
