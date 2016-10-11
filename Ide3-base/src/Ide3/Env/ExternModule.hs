{-|
Module      : Ide3.Env.Module
Description : Operations on the module data sturcture
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Env.ExternModule where

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


import Ide3.Env

import Ide3.Types.Internal
import Ide3.Types.State

import Ide3.Module.Extern.Internal

-- | Add an export and return the id assigned to it
addExternExport :: Monad m => DescentChain2 ExternModule ExternExport m u ExportId
addExternExport = do
    e <- lift ask
    ei <- gets nextExternExportId
    m <- get
    put =<< lift (lift $ addChildT ei e m)
    return ei

-- | Remove an export by id
removeExternExport :: Monad m => DescentChain2 ExternModule ExportId m u ()
removeExternExport = do
    ei <- lift ask
    m <- get
    (e,m') <- lift $ lift $ removeChildT ei m
    let _ = e :: ExternExport
    put m'

-- | Get an export by id
getExternExport :: Monad m => DescentChain2 ExternModule ExportId m u ExternExport
getExternExport = descendRO ask

-- | Get the ids of all exports, or signify that all symbols are exported
getExternExports :: Monad m => DescentChain1 ExternModule m u [ExportId]
getExternExports = gets $ Map.keys . externModuleExports
