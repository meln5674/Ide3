module Ide3.Utils where

import System.IO.Error

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Types


wrapIOError :: (MonadIO m) => IO a -> ProjectResult m u a
wrapIOError = wrapIOErrorWithMsg ""


wrapIOErrorWithMsg :: (MonadIO m) => String -> IO a -> ProjectResult m u a
wrapIOErrorWithMsg msg f = do
    x <- liftIO $ tryIOError f
    case x of
        Right x -> return x
        Left err -> throwE $ InvalidOperation (show err) msg

wrapReadFile :: (MonadIO m) => FilePath -> ProjectResult m u String
wrapReadFile path = wrapIOErrorWithMsg errMsg $ readFile path
  where
    errMsg = "When reading " ++ path
