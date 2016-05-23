{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Free.State.Instances where

import qualified Data.Map as Map
import Data.Map (Map, (!))

import Control.Monad

import qualified Ide3.Import as Import
import qualified Ide3.Export as Export
import qualified Ide3.Project as Project
import qualified Ide3.Module as Module
import qualified Ide3.Declaration as Declaration

import Ide3.Types

import Ide3.Free.State.Classes

hasModuleInfo :: Project -> ModuleInfo -> Bool
hasModuleInfo p mi = mi `elem` (Map.keys $ projectModules p)

hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = di `elem` (Map.keys $ moduleDeclarations m)

hasImportId :: Module -> ImportId -> Bool
hasImportId m ii = ii `elem` (Map.keys $ moduleImports m)

hasExportId :: Module -> ExportId -> Bool
hasExportId m ei = case moduleExports m of
    Nothing -> False
    Just eis -> ei `elem` Map.keys eis

instance ProjectStructure Project where
    createModule mi p = do
        if p `hasModuleInfo` mi
            then Left $ DuplicateModule mi "createModule"
            else do
                Right $ p{projectModules = Map.insert mi (Module.new mi) $ projectModules p}
    removeModule mi p = do
        if p `hasModuleInfo` mi
            then do
                Right $ p{projectModules = Map.delete mi $ projectModules p}
            else Left $ ModuleNotFound mi "removeModule"
    getModules p = Right $ Map.keys $ projectModules p

{-
instance ProjectStructure Project ModuleInfo (ProjectError u) where
    createExternModule mi em p = Project.addExternModule p em
    removeExternModule mi p = Project.removeExternModule p mi
    getExternModules p = Map.keys $ projectExternModules p
-}

instance ProjectEditStructure Project Module where
    editModule mi f p = do
        if p `hasModuleInfo` mi
            then do
                (m',r) <- f $ projectModules p ! mi
                let p' = p{projectModules = Map.update (\_ -> Just m') mi $ projectModules p}
                return (p',r)
            else Left $ ModuleNotFound mi "editModule"
    withModule mi f p = do
        if p `hasModuleInfo` mi
            then f $ projectModules p ! mi
            else Left $ ModuleNotFound mi "withModule"

instance DeclarationStructure Module where
    createDeclaration b m = do
        (WithBody d b') <- Declaration.parseAndCombine b Nothing
        let di = (Declaration.info d)
            newDeclarations = Map.insert di (WithBody d b') $ moduleDeclarations m
            m' = m{ moduleDeclarations = newDeclarations }
        return (m',(di,d))
    editDeclaration di f m = do
        if m `hasDeclarationInfo` di
            then do
                let (WithBody d b) = moduleDeclarations m ! di
                    (di',d',b') = f (di,d,b)
                    ds' = Map.insert di' (WithBody d' b')
                        $ Map.delete di
                        $ moduleDeclarations m
                    m' = m { moduleDeclarations = ds' }
                return (m',())
            else Left $ DeclarationNotFound (Module.info m) di "editDeclaration"
    getDeclaration di m = do
        if m `hasDeclarationInfo` di
            then do
                let (WithBody d b) = moduleDeclarations m ! di
                return (di,d,b)
            else Left $ DeclarationNotFound (Module.info m) di "getDeclaration"

instance DeclarationRemove Module where
    removeDeclaration di m = do
        if m `hasDeclarationInfo` di
            then do
                let ds' = Map.delete di $ moduleDeclarations m
                    m' = m { moduleDeclarations = ds' }
                return (m',())
            else Left $ DeclarationNotFound (Module.info m) di "removeDeclaration"

instance DeclarationGet Module where
    getDeclarations m = Right $ Map.keys $ moduleDeclarations m


instance ImportStructure Module where
    createImport b m = do
        i <- Import.parse b
        let ii = Module.nextImportId m
            is' = Map.insert ii (WithBody i b) $ moduleImports m
            m' = m { moduleImports = is' }
        return (m',(ii,i))
    editImport ii f m = do
        if m `hasImportId` ii
            then do
                let (WithBody i b) = moduleImports m ! ii
                    (ii',i',b') = f (ii,i,b)
                    is' = Map.insert ii' (WithBody i' b')
                        $ Map.delete ii
                        $ moduleImports m
                    m' = m { moduleImports = is' }
                return (m',())
            else Left $ InvalidImportId (Module.info m) ii "editImport"
    getImport ii m = do
        if m `hasImportId` ii
            then do
                let (WithBody i b) = moduleImports m ! ii
                return (ii,i,b)
            else Left $ InvalidImportId (Module.info m) ii "getImport"

instance ImportRemove Module where
    removeImport ii m = do
        if m `hasImportId` ii
            then do
                let is' = Map.delete ii $ moduleImports m
                    m' = m { moduleImports = is' }
                return (m',())
            else Left $ InvalidImportId (Module.info m) ii "removeImport"

instance ImportGet Module where
    getImports m = Right $ Map.keys $ moduleImports m


instance ExportStructure Module where
    createExport b m = do
        e <- Export.parse b
        let ei = Module.nextExportId m
            es = moduleExports m
            es' = case es of
                Nothing -> Map.empty
                Just es -> es
            es'' = Map.insert ei (WithBody e b) es'
            m' = m { moduleExports = Just es'' }
        return (m',(ei,e))
    editExport ei f m = do
        if m `hasExportId` ei
            then do
                let Just es = moduleExports m
                    (WithBody e b) = es ! ei
                    (ei',e',b') = f (ei,e,b)
                    es' = Map.insert ei' (WithBody e' b')
                        $ Map.delete ei
                        $ es
                    m' = m { moduleExports = Just es' }
                return (m',())
            else Left $ InvalidExportId (Module.info m) ei "editExport"
    getExport ei m = do
        if m `hasExportId` ei
            then do
                let Just es = moduleExports m
                    (WithBody e b) = es ! ei
                return (ei,e,b)
             else Left $ InvalidExportId (Module.info m) ei "getExport"

instance ExportRemove Module where
    removeExport ei m = do
        if m `hasExportId` ei
            then do
                let Just es = moduleExports m
                    es' = Map.delete ei es
                    m' = m { moduleExports = Just es' }
                return (m',())
            else Left $ InvalidExportId (Module.info m) ei "removeExport"
            
instance ExportGet Module where
    getExports m = Right $ liftM Map.keys $ moduleExports m

instance ExportAllStructure Module where
    exportAll m = Right $ (m { moduleExports = Nothing },())
