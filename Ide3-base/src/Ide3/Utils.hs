module Ide3.Utils where

import Data.List

import System.IO.Error

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

import Ide3.Types.Internal


wrapIOError :: (MonadIO m) => IO a -> SolutionResult u m a
wrapIOError = wrapIOErrorWithMsg ""


wrapIOErrorWithMsg :: (MonadIO m) => String -> IO a -> SolutionResult u m a
wrapIOErrorWithMsg msg f = do
    r <- liftIO $ tryIOError f
    case r of
        Right result -> return result
        Left err -> throwE $ InvalidOperation (show err) msg

wrapReadFile :: (MonadIO m) => FilePath -> SolutionResult u m String
wrapReadFile path = wrapIOErrorWithMsg errMsg $ readFile path
  where
    errMsg = "When reading " ++ path

replace :: String -> String -> String -> String
replace toReplace replacement str = go str
  where
    go [] = []
    go y@(x:xs)
        | toReplace `isPrefixOf` y = replacement ++ go (drop (length toReplace) y)
        | otherwise = x : go xs

-- | A utility class. Types in this class can insert themselves underneath
-- ExceptT in a monad transformer stack
class MonadBounce t where
    bounce :: (Monad m) => ExceptT e m a -> ExceptT e (t m) a

instance MonadBounce (StateT s) where
    bounce f = ExceptT $ StateT $ \s -> fmap (\a -> (a,s)) $ runExceptT f

instance MonadBounce (ReaderT r) where
    bounce f = ExceptT $ ReaderT $ \r -> runExceptT f

class MonadSplice t where
    splice :: (Monad m) => t m a -> t (ExceptT e m) a

instance MonadSplice (StateT s) where
    splice f = StateT $ \s -> ExceptT $ liftM Right $ runStateT f s

instance MonadSplice (ReaderT r) where
    splice f = ReaderT $ \r -> ExceptT $ liftM Right $ runReaderT f r

-- | Synonym for `.`
(.-.) :: (b -> c) -> (a -> b) -> (a -> c)
(.-.) = (.)

-- | Like `.` but the second function and the composed function take two arguments
(.-..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-..) f g a b = f $ g a b

-- | Like `.` but the second function and the composed function take three arguments
(.-...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.-...) f g a b c = f $ g a b c

-- | Like `.` but the second function and the composed function take four arguments
(.-....) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.-....) f g a b c e = f $ g a b c e
