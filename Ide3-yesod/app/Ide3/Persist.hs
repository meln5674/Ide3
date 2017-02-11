module Ide3.Persist where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Ide3.Types

instance PersistField SolutionInfo where
    toPersistValue (SolutionInfo str) = toPersistValue str
    fromPersistValue str = SolutionInfo <$> fromPersistValue str

instance PersistFieldSql SolutionInfo where
    sqlType _ = SqlString


