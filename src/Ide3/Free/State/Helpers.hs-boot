{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ide3.Free.State.Helpers where

import Data.Proxy

import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Free
import Ide3.Free.State.Classes


bounce :: (Monad m, MonadTrans t, Monad (t m)) => ExceptT e m a -> ExceptT e (t m) a

lift2 :: (Monad m, MonadTrans t, MonadTrans t2, Monad (t2 m), Monad (t (t2 m))) 
      =>  m a -> t (t2 m) a

exceptPure :: (Monad m) => Either e a -> ExceptT e m a

queryProject :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => (project -> Either (ProjectError u) t) 
             -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
             -> ExceptT (ProjectError u) (StateT project m) r

queryModule :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => ModuleInfo
            -> (moduleType -> Either (ProjectError u) t)
            -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
            -> ExceptT (ProjectError u) (StateT project m) r

modifyProject :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
              => (project -> Either (ProjectError u) project)
              -> Free (ProjectAST projectNew projectLoad projectSave) r
              -> ExceptT (ProjectError u) (StateT project m) r

modifyModule :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
             => ModuleInfo 
             -> (moduleType -> Either (ProjectError u) (moduleType,()))
             -> Free (ProjectAST projectNew projectLoad projectSave) r
             -> ExceptT (ProjectError u) (StateT project m) r

modifyModule' :: (IsAProject m project moduleType serial projectNew projectLoad projectSave)
              => ModuleInfo 
              -> (moduleType -> Either (ProjectError u) (moduleType,t))
              -> (t -> Free (ProjectAST projectNew projectLoad projectSave) r)
              -> ExceptT (ProjectError u) (StateT project m) r
