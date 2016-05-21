{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Ide3.Free.State.Classes where

import Control.Monad.Trans.Except

class (Monad m) => InitProject projectInit err m project where
    initProject :: projectInit -> ExceptT err m project

class (Monad m) => LoadProject projectLoad err m serial where
    loadSerialProject :: projectLoad -> ExceptT err m serial

class (Monad m) => SaveProject projectSave err m serial where
    saveSerialProject :: projectSave -> serial -> ExceptT err m ()    

class SerializeProject serial project where
    serializeProject :: project -> serial

class DeserializeProject serial err project where
    deserializeProject :: serial -> Either err project

class DeclarationStructure moduleType 
                        declKey declVal 
                        bodyType 
                        err
                        where
    createDeclaration :: bodyType -> moduleType -> Either err (moduleType,(declKey,declVal))
    editDeclaration :: declKey -> ((declKey,declVal,bodyType) -> (declKey,declVal,bodyType)) -> moduleType -> Either err (moduleType,())
    getDeclaration :: declKey -> moduleType -> Either err (declKey,declVal,bodyType)

class DeclarationRemove moduleType declKey err where
    removeDeclaration :: declKey -> moduleType -> Either err (moduleType,())

class DeclarationGet moduleType declKey err where
    getDeclarations :: moduleType -> Either err [declKey]


class ImportStructure moduleType 
                        importKey importVal 
                        bodyType 
                        err
                        where
    createImport :: bodyType -> moduleType -> Either err (moduleType,(importKey,importVal))
    editImport :: importKey -> ((importKey,importVal,bodyType) -> (importKey,importVal,bodyType)) -> moduleType -> Either err (moduleType,())
    getImport :: importKey -> moduleType -> Either err (importKey,importVal,bodyType)

class ImportRemove moduleType importKey err where
    removeImport :: importKey -> moduleType -> Either err (moduleType,())

class ImportGet moduleType importKey err where
    getImports :: moduleType -> Either err [importKey]

class ExportStructure moduleType 
                        exportKey exportVal 
                        bodyType 
                        err
                        where
    createExport :: bodyType -> moduleType -> Either err (moduleType,(exportKey,exportVal))
    editExport :: exportKey -> ((exportKey,exportVal,bodyType) -> (exportKey,exportVal,bodyType)) -> moduleType -> Either err (moduleType,())
    getExport :: exportKey -> moduleType -> Either err (exportKey,exportVal,bodyType)

class ExportRemove moduleType exportKey err where
    removeExport :: exportKey -> moduleType -> Either err (moduleType,())

class ExportGet moduleType exportKey err where
    getExports :: moduleType -> Either err (Maybe [exportKey])

class ExportAllStructure moduleType err where
    exportAll :: moduleType -> Either err (moduleType,())

class ProjectStructure
            project
            moduleKey
            err
            where
    createModule :: moduleKey -> project -> Either err project
    removeModule :: moduleKey -> project -> Either err project
    getModules :: project -> Either err [moduleKey]
{-
class ProjectExternStructure
            project
            moduleKey
            externModuleVal
            err
            where
    createExternModule :: moduleKey -> externModuleVal -> project -> Either err project
    removeExternModule :: moduleKey -> project -> Either err project
    getExternModules :: project -> Either err [moduleKey]
-}
class ProjectEditStructure project moduleKey moduleType err where
    editModule :: moduleKey -> (moduleType -> Either err (moduleType,a)) -> project -> Either err (project,a)
    withModule :: moduleKey -> (moduleType -> Either err a) -> project -> Either err a

