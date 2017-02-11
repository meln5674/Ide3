module Crypto.PasswordStore.Persist where

import Data.Proxy

import Control.Monad

import Data.ByteString

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Crypto.PasswordStore


instance PersistField Salt where
    toPersistValue = toPersistValue . exportSalt
    fromPersistValue = fromPersistValue >=> return . importSalt

instance PersistFieldSql Salt where
    sqlType _ = sqlType (undefined :: Proxy ByteString)
