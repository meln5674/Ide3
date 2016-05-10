module Builder 
    ( Builder
    , BuilderResult (..)
    , runBuilder
    , stackBuilder
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


data BuilderResult
    = BuildFailed String String
    | BuildSucceeded String String

newtype Builder m u = MkBuilder { runBuilderInternal :: ProjectResult m u BuilderResult }

runBuilder :: Builder m u -> ProjectResult m u BuilderResult
runBuilder = runBuilderInternal

stackBuilder :: (MonadIO m, MonadMask m) => Builder m u
stackBuilder = MkBuilder $ ExceptT $ flip catch handleException $ do
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" ["build"] ""
    case ec of
        ExitSuccess -> return $ Right $ BuildSucceeded out err
        ExitFailure _ -> return $ Right $ BuildFailed out err
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
