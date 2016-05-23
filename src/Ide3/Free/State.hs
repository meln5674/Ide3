{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Ide3.Free.State (interpret) where

import Data.Proxy

import Control.Monad.Free

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Free (ProjectAST (..))

import Ide3.Free.State.Classes
import {-# SOURCE #-} Ide3.Free.State.Helpers

import Ide3.Free.Interpreter

{-
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
             {-, ProjectExternStructure project
                                    moduleKey
                                    externModuleVal
                                    err-}
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
-}
interpret :: forall m project moduleType serial projectNew projectLoad projectSave u r
           . ( IsAProject m project moduleType serial projectNew projectLoad projectSave )
          => Free ( ProjectAST projectNew projectLoad projectSave ) r 
          -> ExceptT (ProjectError u) (StateT project m) r
interpret (Free (CreateProject projectNew next)) = do
    project <- bounce $ initProject projectNew
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


interpret (Free (CreateModule moduleKey next))
    = modifyProject (createModule moduleKey) next
interpret (Free (RemoveModule moduleKey next))
    = modifyProject (removeModule moduleKey) next
{-    
interpret (Free (CreateExternModule moduleKey next))
    = modifyProject (createExternModule moduleKey) next
interpret (Free (RemoveExternModule moduleKey next))
    = modifyProject (removeExternModule moduleKey) next
-}
interpret (Free (CreateDeclaration moduleKey body next))
    = modifyModule' moduleKey (createDeclaration body) next
interpret (Free (EditDeclaration moduleKey declKey f next))
    = modifyModule moduleKey (editDeclaration declKey f) next
interpret (Free (RemoveDeclaration moduleKey declKey next))
    = modifyModule moduleKey (removeDeclaration declKey) next

interpret (Free (AddImport moduleKey body next))
    = modifyModule' moduleKey (createImport body) next
interpret (Free (EditImport moduleKey importKey f next))
    = modifyModule moduleKey (editImport importKey f) next
interpret (Free (RemoveImport moduleKey importKey next))
    = modifyModule moduleKey (removeImport importKey) next


interpret (Free (AddExport moduleKey body next))
    = modifyModule' moduleKey (createExport body) next
interpret (Free (EditExport moduleKey exportKey f next))
    = modifyModule moduleKey (editExport exportKey f) next
interpret (Free (RemoveExport moduleKey exportKey next)) 
    = modifyModule moduleKey (removeExport exportKey) next
interpret (Free (ExportAll moduleKey next)) 
    = modifyModule moduleKey exportAll next


interpret (Free (GetModules next))
    = queryProject getModules next
interpret (Free (GetDeclarations moduleKey next)) 
    = queryModule moduleKey getDeclarations next
interpret (Free (GetImports moduleKey next))
    = queryModule moduleKey getImports next
interpret (Free (GetExports moduleKey next)) 
    = queryModule moduleKey getExports next
interpret (Free (GetDeclaration moduleKey declKey next)) 
    = queryModule moduleKey (getDeclaration declKey) next
interpret (Free (GetImport moduleKey importKey next)) 
    = queryModule moduleKey (getImport importKey) next
interpret (Free (GetExport moduleKey exportKey next)) 
    = queryModule moduleKey (getExport exportKey) next

interpret (Pure x) = return x

{-    project <- lift $ get
    result <- exceptPure
        $ withModule project moduleKey $ f
    interpret (next result)
-}
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
