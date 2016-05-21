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

import Ide3.Free
import Ide3.Free.State.Classes

bounce :: (Monad m, MonadTrans t, Monad (t m)) => ExceptT e m a -> ExceptT e (t m) a

lift2 :: (Monad m, MonadTrans t, MonadTrans t2, Monad (t2 m), Monad (t (t2 m))) 
      =>  m a -> t (t2 m) a

exceptPure :: (Monad m) => Either e a -> ExceptT e m a

queryProject :: forall
             (m :: * -> *) serial err
             project
             projectInit projectLoad projectSave
             moduleKey moduleType
             declKey declVal
             importKey importVal
             exportKey exportVal
             bodyType
             r
             t
           . ( ?proxy :: Proxy (serial,err,moduleType)
             , ?proxy_m :: Proxy m
             , InitProject projectInit err  m project
             , LoadProject projectLoad err m serial
             , SaveProject projectSave err m serial
             , SerializeProject serial project
             , DeserializeProject serial err project
             , DeclarationStructure moduleType 
                                declKey declVal 
                                bodyType 
                                err
             , DeclarationRemove moduleType declKey err
             , DeclarationGet moduleType declKey err
             , ImportStructure moduleType 
                                importKey importVal 
                                bodyType 
                                err
             , ImportRemove moduleType importKey err
             , ImportGet moduleType importKey err
             , ExportStructure moduleType 
                                exportKey exportVal 
                                bodyType 
                                err
             , ExportRemove moduleType exportKey err
             , ExportGet moduleType exportKey err
             , ExportAllStructure moduleType err
             , ProjectStructure project
                                    moduleKey
                                    err
             , ProjectEditStructure project
                                    moduleKey moduleType
                                    err
             )
          => (project -> Either err t) 
          -> (t -> Free
                 ( ProjectAST 
                     projectInit projectLoad projectSave 
                     moduleKey
                     declKey declVal
                     importKey importVal
                     exportKey exportVal
                     bodyType
                 ) r)
          -> ExceptT err (StateT project m) r

queryModule :: forall
             (m :: * -> *) serial err
             project
             projectInit projectLoad projectSave
             moduleKey moduleType
             declKey declVal
             importKey importVal
             exportKey exportVal
             bodyType
             r
             t
           . ( ?proxy :: Proxy (serial,err,moduleType)
             , ?proxy_m :: Proxy m
             , InitProject projectInit err  m project
             , LoadProject projectLoad err m serial
             , SaveProject projectSave err m serial
             , SerializeProject serial project
             , DeserializeProject serial err project
             , DeclarationStructure moduleType 
                                declKey declVal 
                                bodyType 
                                err
             , DeclarationRemove moduleType declKey err
             , DeclarationGet moduleType declKey err
             , ImportStructure moduleType 
                                importKey importVal 
                                bodyType 
                                err
             , ImportRemove moduleType importKey err
             , ImportGet moduleType importKey err
             , ExportStructure moduleType 
                                exportKey exportVal 
                                bodyType 
                                err
             , ExportRemove moduleType exportKey err
             , ExportGet moduleType exportKey err
             , ExportAllStructure moduleType err
             , ProjectStructure project
                                    moduleKey
                                    err
             , ProjectEditStructure project
                                    moduleKey moduleType
                                    err
             )
          => moduleKey
          -> (moduleType -> Either err t) 
          -> (t -> Free
                 ( ProjectAST 
                     projectInit projectLoad projectSave 
                     moduleKey
                     declKey declVal
                     importKey importVal
                     exportKey exportVal
                     bodyType
                 ) r)
          -> ExceptT err (StateT project m) r


modifyProject :: forall
             (m :: * -> *) serial err
             project
             projectInit projectLoad projectSave
             moduleKey moduleType
             declKey declVal
             importKey importVal
             exportKey exportVal
             bodyType
             r
           . ( ?proxy :: Proxy (serial,err,moduleType)
             , ?proxy_m :: Proxy m
             , InitProject projectInit err  m project
             , LoadProject projectLoad err m serial
             , SaveProject projectSave err m serial
             , SerializeProject serial project
             , DeserializeProject serial err project
             , DeclarationStructure moduleType 
                                declKey declVal 
                                bodyType 
                                err
             , DeclarationRemove moduleType declKey err
             , DeclarationGet moduleType declKey err
             , ImportStructure moduleType 
                                importKey importVal 
                                bodyType 
                                err
             , ImportRemove moduleType importKey err
             , ImportGet moduleType importKey err
             , ExportStructure moduleType 
                                exportKey exportVal 
                                bodyType 
                                err
             , ExportRemove moduleType exportKey err
             , ExportGet moduleType exportKey err
             , ExportAllStructure moduleType err
             , ProjectStructure project
                                    moduleKey
                                    err
             , ProjectEditStructure project
                                    moduleKey moduleType
                                    err
             )
          => (project -> Either err project) 
          -> (Free
                 ( ProjectAST 
                     projectInit projectLoad projectSave 
                     moduleKey
                     declKey declVal
                     importKey importVal
                     exportKey exportVal
                     bodyType
                 ) r)
          -> ExceptT err (StateT project m) r

modifyModule :: forall
             (m :: * -> *) serial err
             project
             projectInit projectLoad projectSave
             moduleKey moduleType
             declKey declVal
             importKey importVal
             exportKey exportVal
             bodyType
             r
           . ( ?proxy :: Proxy (serial,err,moduleType)
             , ?proxy_m :: Proxy m
             , InitProject projectInit err  m project
             , LoadProject projectLoad err m serial
             , SaveProject projectSave err m serial
             , SerializeProject serial project
             , DeserializeProject serial err project
             , DeclarationStructure moduleType 
                                declKey declVal 
                                bodyType 
                                err
             , DeclarationRemove moduleType declKey err
             , DeclarationGet moduleType declKey err
             , ImportStructure moduleType 
                                importKey importVal 
                                bodyType 
                                err
             , ImportRemove moduleType importKey err
             , ImportGet moduleType importKey err
             , ExportStructure moduleType 
                                exportKey exportVal 
                                bodyType 
                                err
             , ExportRemove moduleType exportKey err
             , ExportGet moduleType exportKey err
             , ExportAllStructure moduleType err
             , ProjectStructure project
                                    moduleKey
                                    err
             , ProjectEditStructure project
                                    moduleKey moduleType
                                    err
             )
          => moduleKey
          -> (moduleType -> Either err (moduleType,()))
          -> (Free
                 ( ProjectAST 
                     projectInit projectLoad projectSave 
                     moduleKey
                     declKey declVal
                     importKey importVal
                     exportKey exportVal
                     bodyType
                 ) r)
          -> ExceptT err (StateT project m) r

modifyModule' :: forall
              (m :: * -> *) serial err
              project
              projectInit projectLoad projectSave
              moduleKey moduleType
              declKey declVal
              importKey importVal
              exportKey exportVal
              bodyType
              r
              t
            . ( ?proxy :: Proxy (serial,err,moduleType)
              , ?proxy_m :: Proxy m
              , InitProject projectInit err m project
              , LoadProject projectLoad err m serial
              , SaveProject projectSave err m serial
              , SerializeProject serial project
              , DeserializeProject serial err project
              , DeclarationStructure moduleType 
                                 declKey declVal 
                                 bodyType 
                                 err
              , DeclarationRemove moduleType declKey err
              , DeclarationGet moduleType declKey err
              , ImportStructure moduleType 
                                 importKey importVal 
                                 bodyType 
                                 err
              , ImportRemove moduleType importKey err
              , ImportGet moduleType importKey err
              , ExportStructure moduleType 
                                 exportKey exportVal 
                                 bodyType 
                                 err
              , ExportRemove moduleType exportKey err
              , ExportGet moduleType exportKey err
              , ExportAllStructure moduleType err
              , ProjectStructure project
                                     moduleKey
                                     err
              , ProjectEditStructure project
                                     moduleKey moduleType
                                     err
              )
           => moduleKey
           -> (moduleType -> Either err (moduleType,t))
           -> (t -> Free
                  ( ProjectAST 
                      projectInit projectLoad projectSave 
                      moduleKey
                      declKey declVal
                      importKey importVal
                      exportKey exportVal
                      bodyType
                  ) r)
           -> ExceptT err (StateT project m) r
