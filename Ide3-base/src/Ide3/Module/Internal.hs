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

addDeclaration :: Monad m 
               => DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult m u Module
addDeclaration di d m = case Map.lookup di $ moduleDeclarations m of
    Just _ -> throwE $ DuplicateDeclaration (info m) di "Module.addDeclaration"
    Nothing -> return $ m{ moduleDeclarations = Map.insert di d $ moduleDeclarations m }

removeDeclaration :: Monad m
                  => DeclarationInfo
                  -> Module
                  -> SolutionResult m u (WithBody Declaration, Module)
removeDeclaration di m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.removeDeclaration"
    Just d -> return (d, m{ moduleDeclarations = Map.delete di $ moduleDeclarations m })

getDeclaration :: Monad m
               => DeclarationInfo
               -> Module
               -> SolutionResult m u (WithBody Declaration)
getDeclaration di m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.getDeclaration"
    Just d -> return d

setDeclaration :: Monad m
               => DeclarationInfo
               -> DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult m u Module
setDeclaration di di' d' m = case Map.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.setDeclaration"
    Just _ -> return $ m
        { moduleDeclarations
            = Map.insert di' d' 
            $ Map.delete di 
            $ moduleDeclarations m
        }

addImport :: Monad m 
          => ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult m u Module
addImport ii i m = case Map.lookup ii $ moduleImports m of
    Just _ -> throwE $ InternalError "Duplicate import id" "Module.addImport"
    Nothing -> return $ m{ moduleImports = Map.insert ii i $ moduleImports m }

removeImport :: Monad m
             => ImportId
             -> Module
             -> SolutionResult m u (WithBody Import, Module)
removeImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.removeImport"
    Just i -> return (i, m{ moduleImports = Map.delete ii $ moduleImports m })

getImport :: Monad m
          => ImportId
          -> Module
          -> SolutionResult m u (WithBody Import)
getImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.getImport"
    Just i -> return i

setImport :: Monad m
          => ImportId
          -> ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult m u Module
setImport ii ii' i' m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.setImport"
    Just _ -> return $ m
        { moduleImports
            = Map.insert ii' i'
            $ Map.delete ii
            $ moduleImports m
        }

addExport :: Monad m 
          => ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult m u Module
addExport ei e m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Just _ -> throwE $ InternalError "Duplicate export id" "Module.addExport"
        Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e es }
    Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e Map.empty }

removeExport :: Monad m
             => ExportId
             -> Module
             -> SolutionResult m u (WithBody Export, Module)
removeExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't remove export from an export all" "Module.removeExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
        Just e -> return (e, m{ moduleExports = Just $ Map.delete ei es })

getExport :: Monad m
          => ExportId
          -> Module
          -> SolutionResult m u (WithBody Export)
getExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't get export from an export all" "Module.getExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (info m) ei "Module.getExport"
        Just e -> return e

setExport :: Monad m
          => ExportId
          -> ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult m u Module
setExport ei ei' e' m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InternalError "Tried to set an export in an export all" "Module.setExport"
        Just _ -> return $ m
            { moduleExports
                = Just
                $ Map.insert ei' e'
                $ Map.delete ei
                  es
            }
    Nothing -> return $ m { moduleExports = Just $ Map.fromList [(ei',e')] }
