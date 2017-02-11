{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Trans.Resource (runResourceT)

import Yesod

import Control.Monad.Reader
import Control.Monad.Logger (runStderrLoggingT)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Crypto.PasswordStore
import Crypto.PasswordStore.Persist

import Ide3.Types hiding (Solution)

import Ide3.Persist

import Persist
import App
import Types
import Routes
import Handlers

main :: IO ()
main = runStderrLoggingT $ withSqlitePool sqlitePath openConnectionCount $
    \pool -> liftIO $ do
        runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
        warp 3000 $ Ide3Yesod pool

