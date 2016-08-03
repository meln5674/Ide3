module Ide3.Utils where

import System.IO.Error

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types.Internal


wrapIOError :: (MonadIO m) => IO a -> SolutionResult m u a
wrapIOError = wrapIOErrorWithMsg ""


wrapIOErrorWithMsg :: (MonadIO m) => String -> IO a -> SolutionResult m u a
wrapIOErrorWithMsg msg f = do
    r <- liftIO $ tryIOError f
    case r of
        Right result -> return result
        Left err -> throwE $ InvalidOperation (show err) msg

wrapReadFile :: (MonadIO m) => FilePath -> SolutionResult m u String
wrapReadFile path = wrapIOErrorWithMsg errMsg $ readFile path
  where
    errMsg = "When reading " ++ path
