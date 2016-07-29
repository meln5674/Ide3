{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Module.Internal where

import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types

import Ide3.Env

instance ParamEnvClass Module DeclarationInfo (WithBody Declaration) (SolutionError u) where
    addChildT = addDeclaration
    removeChildT = removeDeclaration
    getChildT = getDeclaration
    setChildT = setDeclaration

instance ParamEnvClass Module ImportId (WithBody Import) (SolutionError u) where
    addChildT = addImport
    removeChildT = removeImport
    getChildT = getImport
    setChildT = setImport

instance ParamEnvClass Module ExportId (WithBody Export) (SolutionError u) where
    addChildT = addExport
    removeChildT = removeExport
    getChildT = getExport
    setChildT = setExport
    

-- | Get the identifying information from a module
info :: Module -> ModuleInfo
info = moduleInfo

-- | Create an empty module
empty :: Module
empty = Module (UnamedModule Nothing) [] Map.empty Nothing Map.empty

-- | Create a new module from a ModuleInfo
new :: ModuleInfo -> Module
new i = Module i [] Map.empty Nothing Map.empty

addDeclaration di d m = case Map.lookup di $ moduleDeclarations m of
    Just _ -> throwE $ DuplicateDeclaration (info m) di "Module.addDeclaration"
    Nothing -> return $ m{ moduleDeclarations = Map.insert di d $ moduleDeclarations m }

removeDeclaration di m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.removeDeclaration"
    Just d -> return (d, m{ moduleDeclarations = Map.delete di $ moduleDeclarations m })

getDeclaration di m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.getDeclaration"
    Just d -> return d

setDeclaration di di' d' m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.setDeclaration"
    Just _ -> return $ m
        { moduleDeclarations
            = Map.insert di' d' 
            $ Map.delete di 
            $ moduleDeclarations m
        }

addImport ii i m = case Map.lookup ii $ moduleImports m of
    Just _ -> throwE $ InternalError "Duplicate import id" "Module.addImport"
    Nothing -> return $ m{ moduleImports = Map.insert ii i $ moduleImports m }

removeImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.removeImport"
    Just i -> return (i, m{ moduleImports = Map.delete ii $ moduleImports m })

getImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.getImport"
    Just i -> return i

setImport ii ii' i' m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.setImport"
    Just _ -> return $ m
        { moduleImports
            = Map.insert ii' i'
            $ Map.delete ii
            $ moduleImports m
        }

addExport ei e m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Just _ -> throwE $ InternalError "Duplicate export id" "Module.addExport"
        Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e es }
    Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e Map.empty }

removeExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't remove export from an export all" "Module.removeExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
        Just e -> return (e, m{ moduleExports = Just $ Map.delete ei es })

getExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't get export from an export all" "Module.getExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (info m) ei "Module.getExport"
        Just e -> return e

setExport ei ei' e' m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InternalError "Tried to set an export in an export all" "Module.setExport"
        Just _ -> return $ m
            { moduleExports
                = Just
                $ Map.insert ei' e'
                $ Map.delete ei
                $ es
            }
    Nothing -> return $ m { moduleExports = Just $ Map.fromList [(ei',e')] }
