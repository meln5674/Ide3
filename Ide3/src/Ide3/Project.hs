{-|
Module      : Ide3.Project
Description : Top level operations on the project data structure
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module contains the operations, convienence functions, and simple queries
for working with the Project data type.
-}
module Ide3.Project where

import qualified Data.Map as Map

import Control.Monad

import Ide3.Types
import qualified Ide3.Module as Module

-- |Create an empry project
empty :: Project
empty = Project ProjectInfo Map.empty BuildInfo Map.empty

-- |Create a new project from a ProjectInfo
new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo Map.empty

-- |Given a symbol, find all of the declarations which modify it, tagged with
--  the module they are present in
modifiersOf :: Symbol -> Project -> [ModuleChild DeclarationInfo]
modifiersOf s = concatMap (Module.modifiersOf s) . projectModules

-- |List every declaration in the project, tagged with the modules they are
--  present in
allDeclarations :: Project -> [ModuleChild DeclarationInfo]
allDeclarations = concatMap Module.allDeclarations . projectModules

-- |List every declaration in the project, tagged with the modules they are
--  present in
allDeclarationsIn :: Project -> ModuleInfo -> Either (ProjectError u) [ModuleChild DeclarationInfo]
allDeclarationsIn p mi = liftM Module.allDeclarations $ getModule p mi

-- |List every symbol in the project, tagged with the modules they are
--  present in
allSymbols :: Project -> [ModuleChild Symbol]
allSymbols = concatMap Module.allSymbols . projectModules

-- |Get a list of every module in the project
allModules :: Project -> [ModuleInfo]
allModules = Map.keys . projectModules

-- |Add a module to the project
addModule :: Project -> Module -> Either (ProjectError u) Project
addModule p m@(Module i' _ _ _ _)
  = case Map.lookup i' ms of
    Just _ -> Left $ DuplicateModule i' "Project.addModule" 
    Nothing -> Right $ p{projectModules = Map.insert i' m ms} 
  where
    ms = projectModules p

-- |Create a new module from a ModuleInfo
createModule :: Project -> ModuleInfo -> Either (ProjectError u) Project
createModule p i = addModule p (Module.new i)

addExternModule :: Project -> ExternModule -> Either (ProjectError u) Project
addExternModule p m@(ExternModule i' _)
  = case Map.lookup i' ms of
    Just _ -> Left $ DuplicateModule i' "Project.addExternModule"
    Nothing -> Right $ p{projectExternModules = Map.insert i' m ms}
  where  
   ms = projectExternModules p

-- |Retreive a module using its ModuleInfo
getModule :: Project -> ModuleInfo -> Either (ProjectError u) Module
getModule p i = case Map.lookup i ms of
    Just m -> Right m
    Nothing -> Left $ ModuleNotFound i "Project.getModule"
  where
    ms = projectModules p

getExternModule :: Project -> ModuleInfo -> Either (ProjectError u) ExternModule
getExternModule p i = case Map.lookup i ms of
    Just m -> Right m
    Nothing -> Left $ ModuleNotFound i "Project.getExternModule"
  where
    ms = projectExternModules p

-- |Determine if there is a module that matches the provided ModuleInfo
hasModuleInfo :: Project -> ModuleInfo -> Bool
hasModuleInfo p m = hasLocalModuleInfo p m || hasExternModuleInfo p m

hasLocalModuleInfo :: Project -> ModuleInfo -> Bool
hasLocalModuleInfo p m = case getModule p m of
    Right _ -> True
    Left _ -> False

hasExternModuleInfo :: Project -> ModuleInfo -> Bool
hasExternModuleInfo p m = case getExternModule p m of
    Right _ -> True
    Left _ -> False
    
-- |Determine if there is a module which is exactly equivalent to a provided one
hasModule :: Project -> Module -> Bool
hasModule p (Module i _ _ _ _) = hasModuleInfo p i

hasExternModule :: Project -> ExternModule -> Bool
hasExternModule p (ExternModule i _) = hasModuleInfo p i

-- |Remove a module that has matching ModuleInfo
--  This function will fail if no matching module is found
removeModule :: Project -> ModuleInfo -> Either (ProjectError u) Project
removeModule p i
    | p `hasLocalModuleInfo` i = Right $ p{projectModules = ms'} 
    | otherwise                = Left $ ModuleNotFound i "Project.removeModule"
  where
    ms = projectModules p
    ms' = Map.delete i ms

removeExternModule :: Project -> ModuleInfo -> Either (ProjectError u) Project
removeExternModule p i
    | p `hasExternModuleInfo` i = Right $ p{projectExternModules = ms'} 
    | otherwise                 = Left $ ModuleNotFound i "Project.removeModule"
  where
    ms = projectExternModules p
    ms' = Map.delete i ms

-- |Find the module which matches the provided ModuleInfo,
--  If found, apply the provided transformation.
--  If the transformation succeeds, replace the old module, and return the
--      extraneous result
editModuleR :: Project 
            -> ModuleInfo
            -> (Module -> Either (ProjectError u) (Module,a))
            -> Either (ProjectError u) (Project,a)
editModuleR p i f = do
    m <- getModule p i
    (m',x) <- f m
    let ms' = Map.insert i m' $ projectModules p
    return (p{projectModules = ms'}, x)
    
-- |Same as 'editModuleR', but no extra result is produced by the tranformation
editModule :: Project
           -> ModuleInfo
           -> (Module -> Either (ProjectError u) Module)
           -> Either (ProjectError u) Project
editModule p i f = fst <$> editModuleR p i (\m -> (\r -> (r,())) <$> f m)

-- |Same as 'editModuleR', but the transformation is garunteed to succeed
editModuleR' :: Project
             -> ModuleInfo
             -> (Module -> (Module,a))
             -> Either (ProjectError u) (Project,a)
editModuleR' p i f = editModuleR p i (return . f)

-- |Same as 'editModuleR'', but no extra result is produced by the transformation
editModule' :: Project
            -> ModuleInfo
            -> (Module -> Module)
            -> Either (ProjectError u) Project
editModule' p i f = editModule p i (return . f)

-- |Add an import to a module
addImport :: Project
          -> ModuleInfo
          -> WithBody Import
          -> Either (ProjectError u) (Project,ImportId)
addImport p mi i = editModuleR' p mi $ \m -> Module.addImport m i
-- |Remove an import from a module
--  This function fails if no matching import is found
removeImport :: Project
             -> ModuleInfo
             -> ImportId
             -> Either (ProjectError u) Project
removeImport p mi i = editModule p mi $ \m -> Module.removeImport m i

getImport :: Project
          -> ModuleInfo
          -> ImportId
          -> Either (ProjectError u) (WithBody Import)
getImport p mi iid = getModule p mi >>= \m -> Module.getImport m iid

getImports :: Project -> ModuleInfo -> Either (ProjectError u) [ImportId]
getImports p mi = getModule p mi >>= \m -> return (Module.getImportIds m)


-- |Set a module to export all of its symbols
exportAll :: Project
          -> ModuleInfo
          -> Either (ProjectError u) Project
exportAll p mi = editModule' p mi $ \m -> Module.exportAll m
-- |Add an export to a module
addExport :: Project
          -> ModuleInfo
          -> WithBody Export
          -> Either (ProjectError u) (Project, ExportId)
addExport p mi e = editModuleR' p mi $ \m -> Module.addExport m e
-- |Remove an exporty from a module
--  This function fails if no matching export is found
removeExport :: Project
             -> ModuleInfo
             -> ExportId
             -> Either (ProjectError u) Project
removeExport p mi e = editModule p mi $ \m -> Module.removeExport m e

-- | Set a module to export nothing
-- This function failes if no matching module is found
exportNothing :: Project
              -> ModuleInfo
              -> Either (ProjectError u) Project
exportNothing p mi = editModule' p mi Module.exportNothing

getExports :: Project -> ModuleInfo -> Either (ProjectError u) (Maybe [ExportId])
getExports p mi = getModule p mi >>= \m -> return (Module.getExportIds m)

getExport :: Project
          -> ModuleInfo
          -> ExportId
          -> Either (ProjectError u) (WithBody Export)
getExport p mi eid = getModule p mi >>= \m -> Module.getExport m eid

-- |Add a declaration to a module
addDeclaration :: Project 
               -> ModuleInfo 
               -> WithBody Declaration
               -> Either (ProjectError u) Project
addDeclaration p i d = editModule' p i (`Module.addDeclaration` d)
-- |Remove a declaration from a module
--  This function fails if no matching declaration is found
removeDeclaration :: Project 
                  -> ModuleChild DeclarationInfo 
                  -> Either (ProjectError u) Project
removeDeclaration p (ModuleChild i d)
  = editModule p i (`Module.removeDeclaration` d)

-- |Apply a transformation to a declaration in a project
editDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> (Declaration -> Either (ProjectError u) (WithBody Declaration))
                -> Either (ProjectError u) Project
editDeclaration p (ModuleChild i di) f = do
    mi <- Module.info <$> getModule p i
    editModule p mi (\m -> Module.editDeclaration m di f)

-- |Same as `editDeclaration`, but the transformation is garunteed to succeed
editDeclaration' :: Project
                 -> ModuleChild DeclarationInfo
                 -> (Declaration -> (WithBody Declaration))
                 -> Either (ProjectError u) Project
editDeclaration' p m f = editDeclaration p m (return . f)

-- |Move a declaration from one module to another
moveDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> ModuleInfo
                -> Either (ProjectError u) Project
moveDeclaration p c i' = do
  (ModuleChild _ d) <- getDeclaration p c
  p' <- removeDeclaration p c
  addDeclaration p' i' d

-- |Get a declaration from a module
getDeclaration :: Project 
               -> ModuleChild DeclarationInfo 
               -> Either (ProjectError u) (ModuleChild (WithBody Declaration))
getDeclaration p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration` di)

-- |Same as 'getDeclaration', but discards the body
getDeclaration' :: Project
                -> ModuleChild DeclarationInfo
                -> Either (ProjectError u) (ModuleChild Declaration)
getDeclaration' p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration'` di)

