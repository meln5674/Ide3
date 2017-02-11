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
module Persist where

import Data.Text ( Text )
import Data.ByteString ( ByteString )

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Crypto.PasswordStore ( Salt )
import Crypto.PasswordStore.Persist

import Ide3.Types ( SolutionInfo )

import Ide3.Persist

import Types

openConnectionCount :: Int
openConnectionCount = 10

sqlitePath :: Text
sqlitePath = "db.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Username
    email Text
    passwordHash ByteString
    passwordSalt Salt
    deriving Show
Solution
    name SolutionInfo
    owner UserId
    deriving Show
|]

