module Ide3.Env.Solution where

import qualified Data.Map as Map

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Ide3.Env

import Ide3.Types

import qualified Ide3.Env.Project as Project

new :: SolutionInfo -> Solution
new i = Solution i Map.empty


addProject :: Monad m => DescentChain2 Solution ProjectInfo u m ()
addProject = do
    pi <- lift ask
    s <- get
    put =<< (lift $ lift $ addChild pi (Project.new pi) s)

removeProject :: Monad m => DescentChain2 Solution ProjectInfo u m ()
removeProject = do
    pi <- lift ask
    s <- get
    (p, s') <- lift $ lift $ removeChild pi s
    let p' = p :: Project
    put s'

getProjects :: Monad m => DescentChain1 Solution u m [ProjectInfo]
getProjects = gets $ Map.keys . solutionProjects

getProject :: Monad m => DescentChain2 Solution ProjectInfo u m Project
getProject = descend0 get

editProjectInfo :: Monad m => DescentChain3 Solution ProjectInfo (ProjectInfo -> ProjectInfo) u m ()
editProjectInfo = descend1 Project.editProjectInfo

allModules :: Monad m => DescentChain2 Solution ProjectInfo u m [ModuleInfo]
allModules = descend0 Project.allModules

addModule :: Monad m => DescentChain3 Solution ProjectInfo Module u m ()
addModule = descend1 Project.addModule

createModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m ()
createModule = descend1 Project.createModule

addExternModule :: Monad m => DescentChain3 Solution ProjectInfo ExternModule u m ()
addExternModule = descend1 Project.addExternModule

removeModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m ()
removeModule = descend1 Project.removeModule

getModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m Module
getModule = descend1 Project.getModule

getExternModule :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m ExternModule
getExternModule = descend1 Project.getExternModule

editModule :: Monad m 
           => DescentChain4 
                Solution 
                ProjectInfo 
                ModuleInfo 
                (Module -> Either (SolutionError u) Module) 
                u m ()
editModule = descend2 Project.editModule

addDeclaration :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo (WithBody Declaration) u m ()
addDeclaration = descend2 Project.addDeclaration

removeDeclaration :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo DeclarationInfo u m ()
removeDeclaration = descend2 Project.removeDeclaration

editDeclaration :: Monad m 
                => DescentChain5 
                    Solution 
                    ProjectInfo 
                    ModuleInfo 
                    DeclarationInfo 
                    (Declaration -> Either (SolutionError u) (WithBody Declaration))
                    u m DeclarationInfo
editDeclaration = descend3 Project.editDeclaration

getDeclaration :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo DeclarationInfo u m (WithBody Declaration)
getDeclaration = descend2 Project.getDeclaration

getDeclarations:: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m [DeclarationInfo]
getDeclarations = descend1 Project.getDeclarations

addImport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo (WithBody Import) u m ImportId
addImport = descend2 Project.addImport

removeImport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo ImportId u m ()
removeImport = descend2 Project.removeImport

getImport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo ImportId u m (WithBody Import)
getImport = descend2 Project.getImport

getImports :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m [ImportId]
getImports = descend1 Project.getImports

exportAll :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m ()
exportAll = descend1 Project.exportAll

addExport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo (WithBody Export) u m ExportId
addExport = descend2 Project.addExport

removeExport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo ExportId u m ()
removeExport = descend2 Project.removeExport

exportNothing :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m ()
exportNothing = descend1 Project.exportNothing

getExport :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo ExportId u m (WithBody Export)
getExport = descend2 Project.getExport

getExports :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m (Maybe [ExportId])
getExports = descend1 Project.getExports

addPragma :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo Pragma u m ()
addPragma = descend2 Project.addPragma

removePragma :: Monad m => DescentChain4 Solution ProjectInfo ModuleInfo Pragma u m ()
removePragma = descend2 Project.removePragma

getPragmas :: Monad m => DescentChain3 Solution ProjectInfo ModuleInfo u m [Pragma]
getPragmas = descend1 Project.getPragmas

