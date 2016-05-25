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


data RunnerResult
    = RunFailed String String
    | RunSucceeded String String

newtype Runner m u = MkRunner { runRunnerInternal :: ProjectResult m u RunnerResult }

runRunner :: Runner m u -> ProjectResult m u RunnerResult
runRunner = runRunnerInternal

noRunner :: Monad m => Runner m u
noRunner = MkRunner $ throwE $ Unsupported "No runner specified"

stackRunner :: (MonadIO m, MonadMask m) => Runner m u
stackRunner = MkRunner $ ExceptT $ flip catch handleException $ do
    (_,pkgName,_) <- liftIO $ readProcessWithExitCode "stack" ["ide","packages"] ""
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" ["exec",init pkgName] ""
    case ec of
        ExitSuccess -> return $ Right $ RunSucceeded out err
        ExitFailure _ -> return $ Right $ RunFailed out err
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
