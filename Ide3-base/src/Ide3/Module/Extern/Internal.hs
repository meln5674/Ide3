{-|
Module      : Ide3.Module.Extern.Internal
Description : External modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Module.Extern.Internal where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Trans.Except

import Ide3.Types.State
import Ide3.Types.Internal

import Ide3.Env

-- | Get the identifying information from an external module
info :: ExternModule -> ModuleInfo
info (ExternModule i _) = i

-- | Create an empty external module which exports nothing
new :: ModuleInfo -> ExternModule
new i = ExternModule i Map.empty

-- | Get the next id for exports in an external module
nextId :: (Enum k, Ord k) => k -> Map k v -> k
nextId default_ m = succ $ maximum (pred default_ : Map.keys m)

-- | Get the next value to use for an ImportId
nextExternExportId :: ExternModule -> ExportId
nextExternExportId = nextId 0 . externModuleExports

-- | Add, remove, retrieve, and overwrite external exports
instance ParamEnvClass ExternModule 
                       ExportId 
                       ExternExport 
                       (SolutionError u) where
    addChildT = addExport
    removeChildT = removeExport
    getChildT = getExport
    setChildT = setExport

-- | Add an external export to an external module
addExport :: Monad m 
          => ExportId
          -> ExternExport
          -> ExternModule
          -> SolutionResult u m ExternModule
addExport ei e m = case Map.lookup ei $ externModuleExports m of
    Just _ -> throwE
        $ InternalError "Duplicate export id" "ExternModule.addExport"
    Nothing -> return m
        { externModuleExports = Map.insert ei e $ externModuleExports m }

-- | Remove an external export from an external module
removeExport :: Monad m
             => ExportId
             -> ExternModule
             -> SolutionResult u m (ExternExport, ExternModule)
removeExport ei m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE
        $ InvalidExportId (externModuleInfo m) ei "ExternModule.removeExport"
    Just e -> return
        ( e
        , m{ externModuleExports = Map.delete ei $ externModuleExports m }
        )

-- | Get an external export from an external module
getExport :: Monad m
          => ExportId
          -> ExternModule
          -> SolutionResult u m (ExternExport)
getExport ei m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE $ InvalidExportId (info m) ei "ExternModule.getExport"
    Just e -> return e

-- | Update an external export in an external module
setExport :: Monad m
          => ExportId
          -> ExportId
          -> ExternExport
          -> ExternModule
          -> SolutionResult u m ExternModule
setExport ei ei' e' m = case Map.lookup ei $ externModuleExports m of
    Nothing -> throwE $ InvalidExportId (info m) ei "ExternModule.setExport"
    Just _ -> return m
        { externModuleExports
            = Map.insert ei' e'
            $ Map.delete ei
            $ externModuleExports m
        }
