{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Ide3.Env.Project where

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Env
import Ide3.Types

import qualified Ide3.Env.Module as Module

catchE' :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchE' = catchE

new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo Map.empty

editProjectInfo :: Monad m => DescentChain2 Project (ProjectInfo -> ProjectInfo) u m ()
editProjectInfo = do
    f <- lift ask
    modify $ \p -> p{ projectInfo = f $ projectInfo p }

allModules :: Monad m => DescentChain1 Project u m [ModuleInfo]
allModules = gets $ Map.keys . projectModules

addModule :: Monad m => DescentChain2 Project Module u m ()
addModule = do
    m <- lift ask
    p <- get
    put =<< (lift $ lift $ addChild (moduleInfo m) m p)

createModule :: Monad m => DescentChain2 Project ModuleInfo u m ()
createModule = do
    mi <- lift ask
    p <- get
    put =<< (lift $ lift $ addChild mi (Module.new mi) p)

addExternModule :: Monad m => DescentChain2 Project ExternModule u m ()
addExternModule = do
    p <- get
    m <- lift ask
    put =<< (lift $ lift $ addChild (externModuleInfo m) m p)

removeModule :: Monad m => DescentChain2 Project ModuleInfo u m ()
removeModule = do
    p <- get
    mi <- lift ask
    let removeLocal = do
            (m,p') <- removeChild mi p
            let m' = m :: Module
            return p'
    let removeExtern = do
            (m,p') <- removeChild mi p
            let m' = m :: ExternModule
            return p'
    put =<< (lift $ lift $ catchE' removeExtern $ const removeLocal)

getModule :: Monad m => DescentChain2 Project ModuleInfo u m Module
getModule = descend0 get

getExternModule :: Monad m => DescentChain2 Project ModuleInfo u m ExternModule
getExternModule = descend0 get

editModule :: Monad m 
           => DescentChain3 
                Project 
                ModuleInfo 
                (Module -> Either (SolutionError u) Module) 
                u m ()
editModule = descend1 $ do
    f <- lift ask
    m <- get
    case f m of
        Right m' -> put m'
        Left err -> throw2 err
    

addDeclaration :: Monad m => DescentChain3 Project ModuleInfo (WithBody Declaration) u m ()
addDeclaration = descend1 Module.addDeclaration

getDeclaration :: Monad m => DescentChain3 Project ModuleInfo DeclarationInfo u m (WithBody Declaration)
getDeclaration = descend1 Module.getDeclaration

getDeclarations :: Monad m => DescentChain2 Project ModuleInfo u m [DeclarationInfo]
getDeclarations = descend0 Module.getDeclarations

removeDeclaration :: Monad m => DescentChain3 Project ModuleInfo DeclarationInfo u m ()
removeDeclaration = descend1 Module.removeDeclaration

editDeclaration :: Monad m 
                => DescentChain4 
                    Project 
                    ModuleInfo 
                    DeclarationInfo 
                    (Declaration -> Either (SolutionError u) (WithBody Declaration))
                    u m DeclarationInfo
editDeclaration = descend2 Module.editDeclaration

addImport :: Monad m => DescentChain3 Project ModuleInfo (WithBody Import) u m ImportId
addImport = descend1 Module.addImport

removeImport :: Monad m => DescentChain3 Project ModuleInfo ImportId u m ()
removeImport = descend1 Module.removeImport

getImport :: Monad m => DescentChain3 Project ModuleInfo ImportId u m (WithBody Import)
getImport = descend1 Module.getImport

getImports :: Monad m => DescentChain2 Project ModuleInfo u m [ImportId]
getImports = descend0 Module.getImports

exportAll :: Monad m => DescentChain2 Project ModuleInfo u m ()
exportAll = descend0 Module.exportAll

addExport :: Monad m => DescentChain3 Project ModuleInfo (WithBody Export) u m ExportId
addExport = descend1 Module.addExport

removeExport :: Monad m => DescentChain3 Project ModuleInfo ExportId u m ()
removeExport = descend1 Module.removeExport

exportNothing :: Monad m => DescentChain2 Project ModuleInfo u m ()
exportNothing = descend0 Module.exportNothing

getExport :: Monad m => DescentChain3 Project ModuleInfo ExportId u m (WithBody Export)
getExport = descend1 Module.getExport

getExports :: Monad m => DescentChain2 Project ModuleInfo u m (Maybe [ExportId])
getExports = descend0 Module.getExports

addPragma :: Monad m => DescentChain3 Project ModuleInfo Pragma u m ()
addPragma = descend1 Module.addPragma

removePragma :: Monad m => DescentChain3 Project ModuleInfo Pragma u m ()
removePragma = descend1 Module.removePragma

getPragmas :: Monad m => DescentChain2 Project ModuleInfo u m [Pragma]
getPragmas = descend0 Module.getPragmas

