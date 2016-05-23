{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ide3.Free.State.Instances.SimpleProject where

import Data.Proxy

import qualified Data.Map as Map

import System.Directory
import System.IO.Error

import Text.Read (readEither)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State

import Ide3.Digest

import Ide3.Free.Interpreter
import Ide3.Free.State
import Ide3.Free.State.Classes
import Ide3.Free.State.Instances

import Ide3.Types

data NewProjectArgs
    = NewProject { projectFile :: FilePath, initInfo :: ProjectInfo, initBuild :: BuildInfo }
    | DigestProject { digestDirectory :: FilePath, ifaceFile :: Maybe FilePath }

instance (MonadIO m) => InitProject NewProjectArgs m Project where
    initProject args@NewProject{} = 
        return $ Project (initInfo args) Map.empty (initBuild args) Map.empty
    initProject args@DigestProject{} = do
        digestProject' (digestDirectory args) (ifaceFile args)

instance (MonadIO m) => LoadProject FilePath m String where
    loadSerialProject path = do
        r <- liftIO $ tryIOError $ readFile path
        case r of
            Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Right contents -> return contents

instance (MonadIO m) => SaveProject FilePath m String where
    saveSerialProject path contents = do
        r <- liftIO $ tryIOError $ writeFile path contents
        case r of
            Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Right () -> return ()

instance SerializeProject String Project where
    serializeProject = show

instance DeserializeProject String Project where
    deserializeProject contents = case readEither contents of
        Right project -> return project
        Left err -> Left $ InvalidOperation ("Error on parsing project file: " ++ err) ""




instance (MonadIO m) 
    => ProjectInterpreter (StateT Project m) token u
                           NewProjectArgs FilePath FilePath
                           where
    runInterpreter t ast = do
        r <- interpret ast
        return $ (r,t)
      where
        ?proxy = (undefined :: Proxy (Project, Module, String, NewProjectArgs, FilePath, FilePath))
        ?proxy_m = (undefined :: Proxy m)
