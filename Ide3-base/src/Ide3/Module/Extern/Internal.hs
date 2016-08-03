{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Ide3.Module.Extern.Internal
Description : External modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}
module Ide3.Module.Extern.Internal where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal

import Ide3.Env

-- | Get the identifying information from an external module
info :: ExternModule -> ModuleInfo
info (ExternModule i _) = i

new :: ModuleInfo -> ExternModule
new i = ExternModule i Map.empty

nextId :: (Enum k, Ord k) => k -> Map k v -> k
nextId default_ m = succ $ maximum (pred default_ : Map.keys m)

-- | Get the next value to use for an ImportId
nextExternExportId :: ExternModule -> ExportId
nextExternExportId = nextId 0 . externModuleExports

instance ParamEnvClass ExternModule ExportId ExternExport (SolutionError u) where
    addChildT = addExport
    removeChildT = removeExport
    getChildT = getExport
    setChildT = setExport

addExport :: Monad m 
          => ExportId
          -> ExternExport
          -> ExternModule
          -> SolutionResult m u ExternModule
addExport ei e m = case Map.lookup ei $ externModuleExports m of
    Just _ -> throwE $ InternalError "Duplicate export id" "ExternModule.addExport"
    Nothing -> return $ m{ externModuleExports = Map.insert ei e $ externModuleExports m }

removeExport :: Monad m
             => ExportId
             -> ExternModule
             -> SolutionResult m u (ExternExport, ExternModule)
removeExport ei m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE $ InvalidExportId (externModuleInfo m) ei "ExternModule.removeExport"
    Just e -> return (e, m{ externModuleExports = Map.delete ei $ externModuleExports m })

getExport :: Monad m
          => ExportId
          -> ExternModule
          -> SolutionResult m u (ExternExport)
getExport ei m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE $ InvalidExportId (info m) ei "ExternModule.getExport"
    Just e -> return e

setExport :: Monad m
          => ExportId
          -> ExportId
          -> ExternExport
          -> ExternModule
          -> SolutionResult m u ExternModule
setExport ei ei' e' m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE $ InvalidExportId (info m) ei "ExternModule.setExport"
    Just _ -> return $ m
        { externModuleExports
            = Map.insert ei' e'
            $ Map.delete ei
            $ externModuleExports m
        }