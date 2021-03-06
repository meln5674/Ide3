{-|
Module      : Ide3.Utils
Description : Utilities
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
module Ide3.Utils where

import qualified Data.Text as T

import System.FilePath
import System.IO.Error

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Ide3.Types.Internal


-- | Execute an IO action, if it throws an async exception, lift it into an sync
-- exception with no message
wrapIOError :: (MonadIO m) => IO a -> SolutionResult u m a
wrapIOError = wrapIOErrorWithMsg ""

-- | Execute an IO action, if it throws an async exception, lift it into a sync
-- exception with the given message.
wrapIOErrorWithMsg :: (MonadIO m) => String -> IO a -> SolutionResult u m a
wrapIOErrorWithMsg msg f = do
    r <- liftIO $ tryIOError f
    case r of
        Right result -> return result
        Left err -> throwE $ InvalidOperation (show err) msg

-- | Try to read a file, if it throws an async exception, lift it into a sync
-- exception
wrapReadFile :: (MonadIO m) => FilePath -> SolutionResult u m String
wrapReadFile path = wrapIOErrorWithMsg errMsg $ readFile path
  where
    errMsg = "When reading " ++ path

moduleInfoToPath :: FilePath -> ModuleInfo ->  FilePath
moduleInfoToPath root (ModuleInfo (Symbol s)) = root </> relative <.> "hs"
  where
    relative = T.unpack $ T.intercalate (T.pack [pathSeparator]) $ T.splitOn "." $ s

pathToModuleInfo :: FilePath -> FilePath -> ModuleInfo
pathToModuleInfo rootPath absPath = ModuleInfo $ Symbol s
  where
    relative = dropExtension $ makeRelative rootPath absPath
    s = T.intercalate "." $ T.splitOn (T.pack [pathSeparator]) $ T.pack relative

-- | Class of transformers whch can insert themselves underneath
-- ExceptT in a monad transformer stack
class MonadBounce t where
    -- | Bubble up the exception one level
    bounce :: (Monad m) => ExceptT e m a -> ExceptT e (t m) a


-- | Allow state to fail
instance MonadBounce (StateT s) where
    bounce f = ExceptT $ StateT $ \s -> flip (,) s <$> runExceptT f

-- | Allow reader to fail
instance MonadBounce (ReaderT r) where
    bounce = ExceptT . ReaderT . const . runExceptT

-- | Class of transformers which can insert a dummy ExceptT layer underneath
-- themselves
class MonadSplice t where
    splice :: (Monad m) => t m a -> t (ExceptT e m) a

-- | Allow underlying monad to fail
instance MonadSplice (StateT s) where
    splice f = StateT $ \s -> ExceptT $ Right <$> runStateT f s

-- | Allow underlying monad to fail
instance MonadSplice (ReaderT r) where
    splice f = ReaderT $ \r -> ExceptT $ Right <$> runReaderT f r

-- | Class of transformers which can change an Either value into an ExceptT
-- layer underneath themsevles
class MonadUnsplice t where
    unsplice :: (Monad m) => t m (Either e a) -> t (ExceptT e m) a

-- | Change a Left value into failure
instance MonadUnsplice (ReaderT r) where
    unsplice f = ReaderT $ \r -> ExceptT $ runReaderT f r

class MonadSplice2 t where
    splice2 :: (Monad m, MonadTrans u) => t m a -> t (u m) a

instance MonadSplice2 (ExceptT e) where
    splice2 (ExceptT f) = ExceptT $ lift f

instance MonadSplice2 (StateT s) where
    splice2 (StateT f) = StateT $ \s -> lift $ f s

instance MonadSplice2 (ReaderT r) where
    splice2 (ReaderT f) = ReaderT $ \r -> lift $ f r

-- | Synonym for `.`
(.-.) :: (b -> c) -> (a -> b) -> (a -> c)
(.-.) = (.)

-- | Like `.` but the second function and the composed function take two
-- arguments
(.-..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-..) f g a b = f $ g a b

-- | Like `.` but the second function and the composed function take three
-- arguments
(.-...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.-...) f g a b c = f $ g a b c

-- | Like `.` but the second function and the composed function take four
-- arguments
(.-....) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.-....) f g a b c e = f $ g a b c e
