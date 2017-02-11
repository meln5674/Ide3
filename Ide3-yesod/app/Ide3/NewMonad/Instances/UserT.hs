{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Ide3.NewMonad.Instances.UserT where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad.Reader

import Yesod
import Yesod.Core.Types

import Ide3.NewMonad
import Ide3.Utils
import Ide3.Types

import Database.Persist

import App
import Routes
import Types
import Persist
import UserT

instance MonadBounce (HandlerT site) where
    bounce f = ExceptT $ HandlerT $ const $ runExceptT f

{-
instance MonadSplice2 (HandlerT site) where
    splice2 (HandlerT f) = HandlerT $ \site -> lift $ f site
-}

{-
instance ( PersistenceClass (t m)
         , UserPersistence m
         , MonadTrans t
         , MonadBounce t
         , Monad (t (HandlerT app m))
         ) => PersistenceClass (UserT (ReaderT backend (t (HandlerT app m)))) where
    load = do
        user <- ask
        lift $ lift $ lift $ lift $ lift $ modifyPersistence user
        bounce $ bounce $ bounce $ bounce $ load
    new info = do
        user <- ask
        lift $ lift $ lift $ lift $ lift $ modifyPersistence user
        bounce $ bounce $ bounce $ new info
    finalize = do
        user <- ask
        lift $ lift $ lift $ lift $ lift $ modifyPersistence user
        bounce $ bounce $ bounce $ finalize
-}

instance ( (BaseBackend backend ~ Backend)
         , PersistQueryRead backend
         , PersistStoreWrite backend
         , SolutionClass (t IO)
         , MonadSplice2 t
         , MonadTrans t
         , MonadIO (t Handler)
         ) => SolutionClass (SolutionT (UserT (ReaderT backend (t Handler)))) where
    editSolutionInfo f = do
        userId <- lift $ lift $ getUserId
        solutionId <- lift $ getSolutionId
        [result] <- lift $ lift $ lift $ selectList [SolutionId ==. solutionId] [LimitTo 1]
        lift $ lift $ lift $ update solutionId $ [Update
                { updateField = SolutionName
                , updateValue = f $ solutionName $ entityVal $ result
                , updateUpdate = Assign
                }]
        lift $ lift $ lift $ lift $ splice $ editSolutionInfo f
    new solutionInfo = do
        userId <- lift $ lift $ getUserId
        lift $ lift $ lift $ insert $ Solution solutionInfo userId
        lift $ lift $ lift $ lift $ splice $ new solutionInfo
    finalize = do
        userId <- lift $ lift $ getUserId
        solutionId <- lift $ getSolutionId
        lift $ lift $ lift $ lift $ splice $ finalize
