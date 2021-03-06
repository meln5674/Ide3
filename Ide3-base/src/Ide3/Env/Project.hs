{-|
Module      : Ide3.Env.Project
Description : Operations on the project data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Ide3.Env.Project where

import qualified Data.Map as Map

import Data.Text (Text)

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Env
import Ide3.Types.Internal
import Ide3.Types.State
import Ide3.SrcLoc.Types

import qualified Ide3.Module as Module (new)
import qualified Ide3.Env.Module as Module
import qualified Ide3.Module.Extern as ExternModule
import qualified Ide3.Env.ExternModule as ExternModule

import Ide3.Project.Internal()

-- | Wrapper around catchE that contrains the exception type
catchE' :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchE' = catchE

-- | Edit the info of a project
editProjectInfo :: Monad m 
                => DescentChain2 Project (ProjectInfo -> ProjectInfo) m u ()
editProjectInfo = do
    f <- lift ask
    modify $ \p -> p{ projectInfo = f $ projectInfo p }

-- | Get the ids of all modules
allModules :: Monad m => DescentChain1 Project m u [ModuleInfo]
allModules = gets $ Map.keys . projectModules

-- | Add a local module
addModule :: Monad m => DescentChain2 Project Module m u ()
addModule = do
    m <- lift ask
    p <- get
    put =<< lift (lift $ addChildT (moduleInfo m) m p)

-- | Add an empty local module
createModule :: Monad m => DescentChain2 Project ModuleInfo m u ()
createModule = do
    mi <- lift ask
    p <- get
    put =<< lift (lift $ addChildT mi (Module.new mi) p)

-- | Remove a module
removeModule :: Monad m => DescentChain2 Project ModuleInfo m u ()
removeModule = do
    p <- get
    mi <- lift ask
    (m,p') <- lift $ lift $ removeChildT mi p
    let _ = m :: Module
    put p'

-- | Get a local module by id
getModule :: Monad m => DescentChain2 Project ModuleInfo m u Module
getModule = descend0 get

-- | Apply a transformation to a module
editModule :: Monad m 
           => DescentChain3 
                Project 
                ModuleInfo 
                (Module -> Either (SolutionError u) Module) 
                m u ()
editModule = descend1 $ do
    f <- lift ask
    m <- get
    case f m of
        Right m' -> put m'
        Left err -> throw2 err

-- | Get the header from a module
getModuleHeader :: Monad m
                => DescentChain2 Project ModuleInfo m u Text
getModuleHeader = descend0 $ gets moduleHeader

-- | Edit the header of a module
editModuleHeader :: Monad m
                 => DescentChain3 Project ModuleInfo (Text -> Text) m u ()
editModuleHeader = descend1 $ do
    f <- lift ask
    modify $ \m -> m { moduleHeader = f $ moduleHeader m }

setModuleUnparsable :: Monad m
                     => DescentChain3 Project
                                      ModuleInfo
                                      (Text, SrcLoc, String)
                        m u ()
setModuleUnparsable = descend1 $ do
    m <- get
    (contents, loc, msg) <- lift ask
    put $ UnparsableModule (moduleInfo m) contents loc msg

setModuleParsable :: Monad m
                   => DescentChain2 Project
                                    ModuleInfo
                      m u ()
setModuleParsable = descend0 $ do
    m <- get
    put $ Module.new $ moduleInfo m

getUnparsableModule :: Monad m
                     => DescentChain2 Project
                                      ModuleInfo
                        m u (Maybe (Text, SrcLoc, String))
getUnparsableModule = descend0 $ do
    m <- get
    case m of
        UnparsableModule _ contents loc msg -> return $ Just (contents, loc, msg)
        _ -> return Nothing
        

-- | Add an external module
addExternModule :: Monad m => DescentChain2 Project ExternModule m u ()
addExternModule = do
    p <- get
    m <- lift ask
    put =<< lift (lift $ addChildT (externModuleInfo m) m p)

-- | Add an empty external module
createExternModule :: Monad m => DescentChain2 Project ModuleInfo m u ()
createExternModule = do
    mi <- lift ask
    p <- get
    put =<< lift (lift $ addChildT mi (ExternModule.new mi) p)

-- | Get an external module by id
getExternModule :: Monad m => DescentChain2 Project ModuleInfo m u ExternModule
getExternModule = descend0 get

-- | Get the ids of all external modules
getExternModules :: Monad m => DescentChain1 Project m u [ModuleInfo]
getExternModules = gets $ Map.keys . projectExternModules

-- | Remove an external module from a project
removeExternModule :: Monad m => DescentChain2 Project ModuleInfo m u ()
removeExternModule = do
    p <- get
    mi <- lift ask
    (m,p') <- lift $ lift $ removeChildT mi p
    let _ = m :: ExternModule
    put p'


-- | Add a declaration to a module
addDeclaration :: Monad m 
               => DescentChain3 Project ModuleInfo (WithBody Declaration) m u ()
addDeclaration = descend1 Module.addDeclaration

-- | Get a declaration by id from a module
getDeclaration :: Monad m 
               => DescentChain3 Project 
                                ModuleInfo 
                                DeclarationInfo 
                                m u (WithBody Declaration)
getDeclaration = descend1 Module.getDeclaration

-- | Get the ids of all declarations in a module
getDeclarations :: Monad m 
                => DescentChain2 Project ModuleInfo m u [DeclarationInfo]
getDeclarations = descend0 Module.getDeclarations

-- | Remove a delcaration by id from a module
removeDeclaration :: Monad m 
                  => DescentChain3 Project ModuleInfo DeclarationInfo m u ()
removeDeclaration = descend1 Module.removeDeclaration

-- | Apply a transformation to a declaration in a module
editDeclaration :: Monad m 
                => DescentChain4 Project 
                                 ModuleInfo 
                                 DeclarationInfo 
                                 ( WithBody Declaration 
                                 -> Either (SolutionError u) 
                                           (WithBody Declaration)
                                 )
                                 m u DeclarationInfo
editDeclaration = descend2 Module.editDeclaration

-- | Add an import to a module and return the id assigned to it
addImport :: Monad m 
          => DescentChain3 Project ModuleInfo (WithBody Import) m u ImportId
addImport = descend1 Module.addImport

-- | Remove an import by id from a module
removeImport :: Monad m 
             => DescentChain3 Project ModuleInfo ImportId m u ()
removeImport = descend1 Module.removeImport

-- | Get an import by id from a module
getImport :: Monad m 
          => DescentChain3 Project ModuleInfo ImportId m u (WithBody Import)
getImport = descend1 Module.getImport

-- | Get the ids of all imports in a module
getImports :: Monad m 
           => DescentChain2 Project ModuleInfo m u [ImportId]
getImports = descend0 Module.getImports

-- | Set a module to export all symbols
exportAll :: Monad m => DescentChain2 Project ModuleInfo m u ()
exportAll = descend0 Module.exportAll

-- | Add an export to a module and return the id assigned to it
addExport :: Monad m 
          => DescentChain3 Project ModuleInfo (WithBody Export) m u ExportId
addExport = descend1 Module.addExport

-- | Remove an export by id from a module
removeExport :: Monad m => DescentChain3 Project ModuleInfo ExportId m u ()
removeExport = descend1 Module.removeExport

-- | Set a module to export nothing
exportNothing :: Monad m => DescentChain2 Project ModuleInfo m u ()
exportNothing = descend0 Module.exportNothing

-- | Get an export by id from a module
getExport :: Monad m 
          => DescentChain3 Project ModuleInfo ExportId m u (WithBody Export)
getExport = descend1 Module.getExport

-- | Get ids of all exports in a module
getExports :: Monad m => DescentChain2 Project ModuleInfo m u (Maybe [ExportId])
getExports = descend0 Module.getExports

-- | Add a pragma to a module
addPragma :: Monad m => DescentChain3 Project ModuleInfo Pragma m u ()
addPragma = descend1 Module.addPragma

-- | Remove a pragma from a module
removePragma :: Monad m => DescentChain3 Project ModuleInfo Pragma m u ()
removePragma = descend1 Module.removePragma

-- | Get all pragmas in a module
getPragmas :: Monad m => DescentChain2 Project ModuleInfo m u [Pragma]
getPragmas = descend0 Module.getPragmas

-- | Add an external export and return the id assigned to it
addExternExport :: Monad m 
                => DescentChain3 Project ModuleInfo ExternExport m u ExportId
addExternExport = descend1 ExternModule.addExternExport

-- | Remove an external export by id
removeExternExport :: Monad m 
                   => DescentChain3 Project ModuleInfo ExportId m u ()
removeExternExport = descend1 ExternModule.removeExternExport

-- | Get an external export by id
getExternExport :: Monad m 
                => DescentChain3 Project ModuleInfo ExportId m u ExternExport
getExternExport = descend1 ExternModule.getExternExport

-- | Get the ids of all external exports, or signify that all symbols are 
-- exported
getExternExports :: Monad m => DescentChain2 Project ModuleInfo m u [ExportId]
getExternExports = descend0 ExternModule.getExternExports
