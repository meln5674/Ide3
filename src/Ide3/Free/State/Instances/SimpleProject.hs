{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Free.State.Instances.SimpleProject where

import qualified Data.Map as Map

import System.Directory
import System.IO.Error

import Text.Read

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Ide3.Digest

import Ide3.Free.State.Classes

import Ide3.Types

data NewProjectArgs
    = NewProject { projectFile :: FilePath, initInfo :: ProjectInfo, initBuild :: BuildInfo }
    
data DigestProjectArgs
    = DigestProject { digestDirectory :: FilePath, ifaceFile :: Maybe FilePath }

instance (Monad m) => InitProject NewProjectArgs err m Project where
    initProject args@NewProject{} = 
        return $ Project (initInfo args) Map.empty (initBuild args) Map.empty

instance (MonadIO m) => InitProject DigestProjectArgs (ProjectError u) m Project where
    initProject args@DigestProject{} = do
        digestProject' (digestDirectory args) (ifaceFile args)

instance (MonadIO m) => LoadProject FilePath (ProjectError u) m String where
    loadSerialProject path = do
        r <- liftIO $ tryIOError $ readFile path
        case r of
            Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Right contents -> return contents

instance (MonadIO m) => SaveProject FilePath (ProjectError u) m String where
    saveSerialProject path contents = do
        r <- liftIO $ tryIOError $ writeFile path contents
        case r of
            Left err -> throwE $ InvalidOperation ("Error on opening file: " ++ show err) ""
            Right () -> return ()

instance SerializeProject String Project where
    serializeProject = show

instance DeserializeProject String (ProjectError u) Project where
    deserializeProject contents = case readEither contents of
        Right project -> return project
        Left err -> Left $ InvalidOperation ("Error on parsing project file: " ++ err) ""
