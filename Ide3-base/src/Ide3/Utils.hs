module Ide3.Utils where

import System.IO.Error

import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types


wrapIOError :: (MonadIO m) => IO a -> ProjectResult m u a
wrapIOError = wrapIOErrorWithMsg ""


wrapIOErrorWithMsg :: (MonadIO m) => String -> IO a -> ProjectResult m u a
wrapIOErrorWithMsg msg f = do
    r <- liftIO $ tryIOError f
    case r of
        Right result -> return result
        Left err -> throwE $ InvalidOperation (show err) msg

wrapReadFile :: (MonadIO m) => FilePath -> ProjectResult m u String
wrapReadFile path = wrapIOErrorWithMsg errMsg $ readFile path
  where
    errMsg = "When reading " ++ path
