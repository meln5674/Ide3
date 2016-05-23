{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Ide3.Free.State.Helpers where

import Data.Proxy

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Free
import Ide3.Free.State
import Ide3.Free.State.Classes

bounce :: (Monad m, MonadTrans t, Monad (t m)) => ExceptT e m a -> ExceptT e (t m) a
bounce = ExceptT . lift . runExceptT

lift2 :: (Monad m, MonadTrans t, MonadTrans t2, Monad (t2 m), Monad (t (t2 m))) 
      =>  m a -> t (t2 m) a
lift2 = lift . lift

exceptPure :: (Monad m) => Either e a -> ExceptT e m a
exceptPure = ExceptT . return

queryProject :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => (project -> Either (ProjectError u) t) 
             -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
             -> ExceptT (ProjectError u) (StateT project m) r
queryProject f next = do
    project <- lift $ get
    result <- exceptPure $ f project
    interpret (next result)

queryModule :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => ModuleInfo
            -> (moduleType -> Either (ProjectError u) t)
            -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
            -> ExceptT (ProjectError u) (StateT project m) r
queryModule moduleKey f next = queryProject (withModule moduleKey f) next

modifyProject :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
              => (project -> Either (ProjectError u) project)
              -> Free (ProjectAST projectNew projectLoad projectSave) r
              -> ExceptT (ProjectError u) (StateT project m) r
modifyProject f next = do
    project <- lift $ get
    project' <- exceptPure $ f project
    lift $ put project'
    interpret next

modifyModule :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => ModuleInfo 
             -> (moduleType -> Either (ProjectError u) (moduleType,()))
             -> Free (ProjectAST projectNew projectLoad projectSave) r
             -> ExceptT (ProjectError u) (StateT project m) r
modifyModule moduleKey f next = do
    project <- lift $ get
    (project',()) <- exceptPure
        $ editModule moduleKey
            ( \(module_ :: moduleType) -> f module_) project
    lift $ put project'
    interpret next


modifyModule' :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
              => ModuleInfo 
              -> (moduleType -> Either (ProjectError u) (moduleType,t))
              -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
              -> ExceptT (ProjectError u) (StateT project m) r
modifyModule' moduleKey f next = do
    project <- lift $ get
    (project',result) <- exceptPure
        $ editModule moduleKey (\(module_ :: moduleType) -> f module_) project
    lift $ put project'
    interpret (next result)
