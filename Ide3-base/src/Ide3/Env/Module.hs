{-|
Module      : Ide3.Env.Module
Description : Operations on the module data sturcture
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Env.Module where

import Data.List

import qualified Data.Map as Map
import qualified Ide3.OrderedMap as OMap

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


import Ide3.Env

import Ide3.Types.Internal
import Ide3.Types.State

import qualified Ide3.Declaration as Declaration

import Ide3.Module

-- | Add a declaration
addDeclaration :: Monad m => DescentChain2 Module (WithBody Declaration) m u ()
addDeclaration = do
    d <- lift ask
    m <- get
    put =<< lift (lift $ addChildT (Declaration.info $ item d) d m)

-- | Get a declaration by id
getDeclaration :: Monad m 
               => DescentChain2 Module 
                                DeclarationInfo 
                                m u (WithBody Declaration)
getDeclaration = descend0 get

-- | Remove a declaration by id
removeDeclaration :: Monad m => DescentChain2 Module DeclarationInfo m u ()
removeDeclaration = do
    di <- lift ask
    m <- get
    (d, m') <- lift $ lift $ removeChildT di m 
    let _ = d :: WithBody Declaration
    put m' 

-- | Get the ids of all declarations
getDeclarations :: Monad m => DescentChain1 Module m u [DeclarationInfo]
getDeclarations = gets $ OMap.keys . moduleDeclarations

-- | Apply a transformation to a declaration
editDeclaration :: Monad m 
                => DescentChain3 
                    Module
                    DeclarationInfo 
                    (  WithBody Declaration 
                    -> Either (SolutionError u) (WithBody Declaration)
                    )
                    m u DeclarationInfo
editDeclaration = descend1 $ do
    f <- lift ask
    d <- get
    case f d of
        Right d' -> do
            put d'
            return $ Declaration.info $ item d'
        Left err -> throw2 err


-- | Add an import and return the id assigned to it
addImport :: Monad m => DescentChain2 Module (WithBody Import) m u ImportId
addImport = do
    i <- lift ask
    ii <- gets nextImportId
    m <- get
    put =<< lift (lift $ addChildT ii i m)
    return ii

-- | Remove an import by id
removeImport :: Monad m => DescentChain2 Module ImportId m u ()
removeImport = do
    ii <- lift ask
    m <- get
    (i,m') <- lift $ lift $ removeChildT ii m
    let _ = i :: WithBody Import
    put m'

-- | Get an import by id
getImport :: Monad m => DescentChain2 Module ImportId m u (WithBody Import)
getImport = descendRO ask

-- | Get the ids of all imports
getImports :: Monad m => DescentChain1 Module m u [ImportId]
getImports = get >>= lift . withParsableF' (Map.keys . moduleImports)


-- | Set the module to export all of its symbols
exportAll :: Monad m => DescentChain1 Module m u ()
exportAll = modify $ \m -> m{ moduleExports = Nothing }

-- | Add an export and return the id assigned to it
addExport :: Monad m => DescentChain2 Module (WithBody Export) m u ExportId
addExport = do
    e <- lift ask
    ei <- gets nextExportId
    m <- get
    put =<< lift (lift $ addChildT ei e m)
    return ei

-- | Remove an export by id
removeExport :: Monad m => DescentChain2 Module ExportId m u ()
removeExport = do
    ei <- lift ask
    m <- get
    (e,m') <- lift $ lift $ removeChildT ei m
    let _ = e :: WithBody Export
    put m'

-- | Remove all exports
exportNothing :: Monad m => DescentChain1 Module m u ()
exportNothing = modify $ \m -> m{ moduleExports = Just Map.empty }

-- | Get an export by id
getExport :: Monad m => DescentChain2 Module ExportId m u (WithBody Export)
getExport = descendRO ask

-- | Get the ids of all exports, or signify that all symbols are exported
getExports :: Monad m => DescentChain1 Module m u (Maybe [ExportId])
getExports = gets $ fmap Map.keys . moduleExports

-- | Add a pragma
addPragma :: Monad m => DescentChain2 Module Pragma m u ()
addPragma = do
    p <- lift ask
    modify $ \m -> m{ modulePragmas = p : modulePragmas m }

-- | Remove a pragma
removePragma :: Monad m => DescentChain2 Module Pragma m u ()
removePragma = do
    p <- lift ask
    modify $ \m -> m{ modulePragmas = delete p $ modulePragmas m }

-- | Get all pragmas
getPragmas :: Monad m => DescentChain1 Module m u [Pragma]
getPragmas = gets modulePragmas
