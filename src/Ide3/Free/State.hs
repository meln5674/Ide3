{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Ide3.Free.State where

import Text.Read (readEither)

import Data.Proxy

import Data.ByteString

import Data.Serialize (Serialize, encode, decode)

import Control.Monad.Free

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Free (ProjectAST (..), ProjectMonad (..))

class (Monad m) => InitProject projectInit m project where
    initProject :: projectInit -> m project

class (Monad m) => LoadProject projectLoad err m serial where
    loadSerialProject :: projectLoad -> ExceptT err m serial

class (Monad m) => SaveProject projectSave err m serial where
    saveSerialProject :: projectSave -> serial -> ExceptT err m ()    

class SerializeProject serial project where
    serializeProject :: project -> serial

class DeserializeProject serial err project where
    deserializeProject :: serial -> Either err project

class ModuleStructure moduleType 
                        declKey declVal 
                        importKey importVal 
                        exportKey exportVal
                        bodyType 
                        err
                        where
    createDeclaration :: moduleType -> bodyType -> Either err (moduleType,declKey,declVal)
    editDeclaration :: moduleType -> declKey -> ((declKey,declVal,bodyType) -> (declKey,declVal,bodyType)) -> Either err moduleType
    removeDeclaration :: moduleType -> declKey -> Either err moduleType

class ProjectStructure
            project
            moduleKey
            err
            where
    createModule :: project -> moduleKey -> Either err project
    removeModule :: project -> moduleKey -> Either err project

class ProjectEditStructure project moduleKey moduleType err where
    editModule :: project -> moduleKey -> (moduleType -> Either err moduleType) -> Either err project

bounce :: (Monad m, MonadTrans t, Monad (t m)) => ExceptT e m a -> ExceptT e (t m) a
bounce = ExceptT . lift . runExceptT

lift2 :: (Monad m, MonadTrans t, MonadTrans t2, Monad (t2 m), Monad (t (t2 m))) 
      =>  m a -> t (t2 m) a
lift2 = lift . lift

interpret :: forall
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
             , InitProject projectInit m project
             , LoadProject projectLoad err m serial
             , SaveProject projectSave err m serial
             , SerializeProject serial project
             , DeserializeProject serial err project
             , ModuleStructure moduleType 
                                declKey declVal 
                                importKey importVal 
                                exportKey exportVal 
                                bodyType 
                                err
             , ProjectStructure project
                                    moduleKey
                                    err
             , ProjectEditStructure project
                                    moduleKey moduleType
                                    err
             )
          => Free
                 ( ProjectAST 
                     projectInit projectLoad projectSave 
                     moduleKey
                     declKey declVal
                     importKey importVal
                     exportKey exportVal
                     bodyType
                 ) r
          -> ExceptT err (StateT project m) r
interpret (Free (CreateProject projectNew next)) = do
    project <- lift2 $ initProject projectNew
    lift $ put project
    interpret next
interpret (Free (LoadProject projectLoad next)) = do
    (serial :: serial) <- bounce $ loadSerialProject projectLoad
    case deserializeProject serial of
        Right project -> do
            lift $ put project
            interpret next
        Left err -> throwE err
interpret (Free (SaveProject projectSave next)) = do
    (serial :: serial) <- lift $ gets serializeProject
    bounce $ saveSerialProject projectSave serial
    interpret next
interpret (Free (CreateModule moduleKey next)) = do
    project <- lift $ get
    project' <- ExceptT $ return $ createModule project moduleKey 
    lift $ put project'
    interpret next
interpret (Free (RemoveModule moduleKey next)) = do
    project <- lift $ get
    project' <- ExceptT $ return $ removeModule project moduleKey
    lift $ put project'
    interpret next

{-    
class StringProject err project where
    projectToString :: project -> String
    projectFromString :: String -> Either err project

class ByteStringProject err project where
    projectToByteString :: project -> ByteString
    projectFromByteString :: ByteString -> Either err project


instance (Show project, Read project) => StringProject String project where
    projectToString = show
    projectFromString = readEither

instance (Serialize project) => ByteStringProject String project where
    projectToByteString = encode
    projectFromByteString = decode

instance (StringProject err project) => SerialProject String err project where
    serializeProject :: (StringProject err project) => project -> String
    serializeProject = projectToString
    deserializeProject = projectFromString

instance (ByteStringProject err project) => SerialProject ByteString err project where
    serializeProject = projectToByteString
    deserializeProject = projectFromByteString

-}
