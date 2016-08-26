{-|
Module      : Builder
Description : Building solutions
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

A Builder is an abstract data type which attempts to build a solution
-}
module Builder 
    ( Builder
    , BuilderResult (..)
    , runBuilder
    , noBuilder
    , stackBuilder
    )
    where

import Control.Exception.Base hiding (catch)

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Catch

import System.Exit
import System.Process

import Ide3.NewMonad
import Ide3.Types

-- | The result of a build operation
data BuilderResult
    = BuildFailed String String -- ^ Build failed, accompanied by stdout and stderr
    | BuildSucceeded String String -- ^ Build succeeded, accompanied by stdout and stderr

-- | The builder abstract type. Use runBuilder to execute the actions of a Builder.
-- A build can failed in one of two ways. A ExceptT Left value indicates that
-- the build could not start, did not complete, etc. A BuildFailed value indicates
-- that the build went through but did not compile or link successfully.
newtype Builder m u = MkBuilder { runBuilderInternal :: SolutionResult u m BuilderResult }

-- | Execute the actions of a builder inside a monad.
runBuilder :: (Monad m) => Builder m u -> SolutionResult u m BuilderResult
runBuilder = runBuilderInternal

-- | A builder which represents having no build capabilities and will always result in an erro
noBuilder :: Monad m => Builder m u
noBuilder = MkBuilder $ throwE $ Unsupported "No builder specified"

-- | A builder which uses stack to build a stack solution
stackBuilder :: (MonadIO m, MonadMask m) => Builder m u
stackBuilder = MkBuilder $ ExceptT $ flip catch handleException $ do
    (ec, out, err) <- liftIO $ readProcessWithExitCode "stack" ["build"] ""
    case ec of
        ExitSuccess -> return $ Right $ BuildSucceeded out err
        ExitFailure _ -> return $ Right $ BuildFailed out err
  where
    handleException e = return $ Left $ InvalidOperation (show (e :: IOException)) ""
