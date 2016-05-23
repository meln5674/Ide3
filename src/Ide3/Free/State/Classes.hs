{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Ide3.Free.State.Classes where

import Data.Proxy

import Ide3.Types

import Control.Monad.Trans.Except

class (Monad m) => InitProject projectInit m project where
    initProject :: projectInit -> ExceptT (ProjectError u) m project

class (Monad m) => LoadProject projectLoad m serial where
    loadSerialProject :: projectLoad -> ExceptT (ProjectError u) m serial

class (Monad m) => SaveProject projectSave m serial where
    saveSerialProject :: projectSave -> serial -> ExceptT (ProjectError u) m ()    

class SerializeProject serial project where
    serializeProject :: project -> serial

class DeserializeProject serial project where
    deserializeProject :: serial -> Either (ProjectError u) project

class DeclarationStructure moduleType where
    createDeclaration :: String 
                      -> moduleType 
                      -> Either (ProjectError u) (moduleType,(DeclarationInfo,Declaration))
    editDeclaration :: DeclarationInfo 
                    -> ((DeclarationInfo,Declaration,String) 
                        -> (DeclarationInfo,Declaration,String)) 
                    -> moduleType 
                    -> Either (ProjectError u) (moduleType,())
    getDeclaration :: DeclarationInfo 
                   -> moduleType 
                   -> Either (ProjectError u) (DeclarationInfo,Declaration,String)

class DeclarationRemove moduleType where
    removeDeclaration :: DeclarationInfo 
                      -> moduleType 
                      -> Either (ProjectError u) (moduleType,())

class DeclarationGet moduleType where
    getDeclarations :: moduleType -> Either (ProjectError u) [DeclarationInfo]


class ImportStructure moduleType where
    createImport :: String 
                 -> moduleType 
                 -> Either (ProjectError u) (moduleType,(ImportId,Import))
    editImport :: ImportId 
               -> ((ImportId,Import,String) 
                    -> (ImportId,Import,String)) 
               -> moduleType 
               -> Either (ProjectError u) (moduleType,())
    getImport :: ImportId 
              -> moduleType 
              -> Either (ProjectError u) (ImportId,Import,String)

class ImportRemove moduleType where
    removeImport :: ImportId 
                 -> moduleType 
                 -> Either (ProjectError u) (moduleType,())

class ImportGet moduleType  where
    getImports :: moduleType 
               -> Either (ProjectError u) [ImportId]

class ExportStructure moduleType where
    createExport :: String 
                 -> moduleType 
                 -> Either (ProjectError u) (moduleType,(ExportId,Export))
    editExport :: ExportId 
               -> ((ExportId,Export,String) 
               -> (ExportId,Export,String)) 
               -> moduleType 
               -> Either (ProjectError u) (moduleType,())
    getExport :: ExportId 
              -> moduleType 
              -> Either (ProjectError u) (ExportId,Export,String)

class ExportRemove moduleType where
    removeExport :: ExportId 
                 -> moduleType 
                 -> Either (ProjectError u) (moduleType,())

class ExportGet moduleType where
    getExports :: moduleType 
               -> Either (ProjectError u) (Maybe [ExportId])

class ExportAllStructure moduleType where
    exportAll :: moduleType 
              -> Either (ProjectError u) (moduleType,())

class ProjectStructure project where
    createModule :: ModuleInfo -> project -> Either (ProjectError u) project
    removeModule :: ModuleInfo -> project -> Either (ProjectError u) project
    getModules :: project -> Either (ProjectError u) [ModuleInfo]

class ProjectEditStructure project moduleType where
    editModule :: ModuleInfo 
               -> (moduleType -> Either (ProjectError u) (moduleType,a)) 
               -> project -> Either (ProjectError u) (project,a)
    withModule :: ModuleInfo 
               -> (moduleType -> Either (ProjectError u) a) 
               -> project 
               -> Either (ProjectError u) a

type IsAProject m project moduleType serial projectNew projectLoad projectSave
    = ( ?proxy :: Proxy (project,moduleType,serial,projectNew,projectLoad,projectSave)
      , ?proxy_m :: Proxy m
      , Monad m
      , InitProject projectNew m project
      , LoadProject projectLoad m serial
      , SaveProject projectSave m serial
      , SerializeProject serial project
      , DeserializeProject serial project
      , DeclarationStructure moduleType 
      , DeclarationRemove moduleType
      , DeclarationGet moduleType
      , ImportStructure moduleType 
      , ImportRemove moduleType
      , ImportGet moduleType
      , ExportStructure moduleType 
      , ExportRemove moduleType
      , ExportGet moduleType
      , ExportAllStructure moduleType
      , ProjectStructure project
      , ProjectEditStructure project moduleType
      )
