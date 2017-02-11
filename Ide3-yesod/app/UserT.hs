{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UserT where

import Control.Monad.Reader

import Database.Persist

import Ide3.Types
import Ide3.Utils

import Persist
import Types

newtype UserT m a = UserT { unUserT :: ReaderT (Entity User) m a }
  deriving (Functor, Applicative, Monad, MonadReader (Entity User), MonadTrans, MonadBounce)

newtype SolutionT m a = SolutionT { unSolutionT :: ReaderT (Entity Solution) m a }
  deriving (Functor, Applicative, Monad, MonadReader (Entity Solution), MonadTrans, MonadBounce)

runUserT :: UserT m a -> Entity User -> m a
runUserT = runReaderT . unUserT

getUserId :: Monad m => UserT m UserId
getUserId = asks entityKey

getUser :: Monad m => UserT m User
getUser = asks entityVal

runSolutionT :: SolutionT m a -> Entity Solution -> m a
runSolutionT = runReaderT . unSolutionT

getSolutionId :: Monad m => SolutionT m SolutionId
getSolutionId = asks entityKey

getSolution :: Monad m => SolutionT m Solution
getSolution = asks entityVal

class Monad m => UserPersistence m where
    modifyPersistence :: Entity User -> m ()
