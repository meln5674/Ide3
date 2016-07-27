module Ide3.Env.Module where

import Data.List

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


import Ide3.Env

import Ide3.Types

import qualified Ide3.Declaration as Declaration

new :: ModuleInfo -> Module
new i = Module i [] Map.empty Nothing Map.empty

nextImportId :: Module -> ImportId
nextImportId m = 1 + maximum (-1 : (Map.keys $ moduleImports m))

nextExportId :: Module -> ExportId
nextExportId m = case moduleExports m of
    Nothing -> 0
    Just es -> 1 + maximum (-1 : Map.keys es)

addDeclaration :: Monad m => DescentChain2 Module (WithBody Declaration) u m ()
addDeclaration = do
    d <- lift $ ask
    m <- get
    put =<< (lift $ lift $ addChild (Declaration.info $ item d) d m)

getDeclaration :: Monad m => DescentChain2 Module DeclarationInfo u m (WithBody Declaration)
getDeclaration = descend0 $ get

removeDeclaration :: Monad m => DescentChain2 Module DeclarationInfo u m ()
removeDeclaration = do
    di <- lift ask
    m <- get
    (d,m') <- lift $ lift $ removeChild di m
    let d' = d :: WithBody Declaration
    put m'

getDeclarations :: Monad m => DescentChain1 Module u m [DeclarationInfo]
getDeclarations = gets $ Map.keys . moduleDeclarations

editDeclaration :: Monad m 
                => DescentChain3 
                    Module 
                    DeclarationInfo 
                    (Declaration -> Either (SolutionError u) (WithBody Declaration))
                    u m DeclarationInfo
editDeclaration = descend1 $ do
    f <- lift ask
    d <- get
    case f $ item d of
        Right d' -> do
            put d'
            return $ Declaration.info $ item d'
        Left err -> throw2 err

addImport :: Monad m => DescentChain2 Module (WithBody Import) u m ImportId
addImport = do
    i <- lift $ ask
    ii <- gets nextImportId
    m <- get
    put =<< (lift $ lift $ addChild ii i m)
    return ii

removeImport :: Monad m => DescentChain2 Module ImportId u m ()
removeImport = do
    ii <- lift $ ask
    m <- get
    (i,m') <- lift $ lift $ removeChild ii m
    let i' = i :: WithBody Import
    put m'

getImport :: Monad m => DescentChain2 Module ImportId u m (WithBody Import)
getImport = descendRO ask

getImports :: Monad m => DescentChain1 Module u m [ImportId]
getImports = gets $ Map.keys . moduleImports

exportAll :: Monad m => DescentChain1 Module u m ()
exportAll = modify $ \m -> m{ moduleExports = Nothing }

addExport :: Monad m => DescentChain2 Module (WithBody Export) u m ExportId
addExport = do
    e <- lift ask
    ei <- gets nextExportId
    m <- get
    put =<< (lift $ lift $ addChild ei e m)
    return ei

removeExport :: Monad m => DescentChain2 Module ExportId u m ()
removeExport = do
    ei <- lift ask
    m <- get
    (e,m') <- lift $ lift $ removeChild ei m
    let e' = e :: WithBody Export
    put m'

exportNothing :: Monad m => DescentChain1 Module u m ()
exportNothing = modify $ \m -> m{ moduleExports = Just Map.empty }

getExport :: Monad m => DescentChain2 Module ExportId u m (WithBody Export)
getExport = descendRO ask

getExports :: Monad m => DescentChain1 Module u m (Maybe [ExportId])
getExports = gets $ fmap Map.keys . moduleExports

addPragma :: Monad m => DescentChain2 Module Pragma u m ()
addPragma = do
    p <- lift ask
    modify $ \m -> m{ modulePragmas = p : modulePragmas m }

removePragma :: Monad m => DescentChain2 Module Pragma u m ()
removePragma = do
    p <- lift ask
    modify $ \m -> m{ modulePragmas = delete p $ modulePragmas m }

getPragmas :: Monad m => DescentChain1 Module u m [Pragma]
getPragmas = gets modulePragmas

