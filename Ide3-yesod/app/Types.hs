{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Text ( Text )

import Yesod

import Database.Persist.Sqlite

import Ide3.Types

data Ide3Yesod = Ide3Yesod ConnectionPool

type Backend = YesodPersistBackend Ide3Yesod

newtype Username = Username { getUsername :: Text }
  deriving ( Eq, Ord )

deriving instance PersistField Username
deriving instance PersistFieldSql Username
deriving instance PathPiece Username
deriving instance Show Username
deriving instance Read Username

data AddUserError
    = UserAlreadyExists
  deriving Show    

data SignInError
    = AuthorizationError
    | NoUsername
    | NoPassword
  deriving Show

data SignUpData = SignUpData 
    { signupUsername :: Username
    , signupEmail :: Text
    , signupPassword :: Text
    }

data SignInData = SignInData
    { signInUsername :: Username
    , signInPassword :: Text
    }

data NewSolutionData = NewSolutionData
    { newSolutionInfo :: SolutionInfo 
    }

data SolutionUserError
    = SolutionUserError
  deriving Show
