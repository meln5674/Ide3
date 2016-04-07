{-|
Module      : Ide3.Project
Description : Top level operations on the project data structure
Copyright   : (c) Andrew Melnick, 2016

Lisense     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module contains the operations, convienence functions, and simple queries
for working with the Project data type.
-}
module Ide3.Project where

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

import Ide3.Types
import qualified Ide3.Module as Module

-- |Create an empry project
empty :: Project
empty = Project ProjectInfo Map.empty BuildInfo

-- |Create a new project from a ProjectInfo
new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo

-- |Given a symbol, find all of the declarations which modify it, tagged with
--  the module they are present in
modifiersOf :: Symbol -> Project -> [ModuleChild DeclarationInfo]
modifiersOf s (Project _ ms _) = concatMap (Module.modifiersOf s) ms

-- |List every declaration in the project, tagged with the modules they are
--  present in
allDeclarations :: Project -> [ModuleChild DeclarationInfo]
allDeclarations (Project _ ms _) = concatMap Module.allDeclarations ms

-- |List every symbol in the project, tagged with the modules they are
--  present in
allSymbols :: Project -> [ModuleChild Symbol]
allSymbols (Project _ ms _) = concatMap Module.allSymbols ms

-- |Add a module to the project
addModule :: Project -> Module -> Either ProjectError Project
addModule (Project i ms b) m@(Module i' _ _ _)
  = case Map.lookup i' ms of
    Just _ -> Left $ "Project.addModule: " ++ (show i') ++ " Is already an existing module"
    Nothing -> Right $ Project i (Map.insert i' m ms) b

-- |Create a new module from a ModuleInfo
createModule :: Project -> ModuleInfo -> Either ProjectError Project
createModule p i = addModule p (Module.new i)

-- |Retreive a module using its ModuleInfo
getModule :: Project -> ModuleInfo -> Either ProjectError Module
getModule (Project _ ms _) i = case Map.lookup i ms of
    Just m -> Right m
    Nothing -> Left $ "Project.getModule: " ++ (show i) ++ " did not match any modules"

-- |Determine if there is a module that matches the provided ModuleInfo
hasModuleInfo :: Project -> ModuleInfo -> Bool
hasModuleInfo p m = case getModule p m of
    Right _ -> True
    Left _ -> False

-- |Determine if there is a module which is exactly equivalent to a provided one
hasModule :: Project -> Module -> Bool
hasModule p (Module i _ _ _) = hasModuleInfo p i

-- |Remove a module that has matching ModuleInfo
--  This function will fail if no matching module is found
removeModule :: Project -> ModuleInfo -> Either ProjectError Project
removeModule p@(Project pi ms b) i
    | p `hasModuleInfo` i = Right $ Project pi ms' b
    | otherwise           = Left $ "Project.removeModule: " ++ (show i) ++ " did not match any modules"
  where
    ms' = Map.delete i ms

-- |Find the module which matches the provided ModuleInfo,
--  If found, apply the provided transformation.
--  If the transformation succeeds, replace the old module, and return the
--      extraneous result
editModuleR :: Project 
            -> ModuleInfo
            -> (Module -> Either ProjectError (Module,a))
            -> Either ProjectError (Project,a)
editModuleR p@(Project pi ms b) i f = do
    m <- getModule p i
    (m',x) <- f m
    let ms' = Map.insert i m' ms
    return $ (Project pi ms' b, x)

-- |Same as 'editModuleR', but no extra result is produced by the tranformation
editModule :: Project
           -> ModuleInfo
           -> (Module -> Either ProjectError Module)
           -> Either ProjectError Project
editModule p i f = fst <$> editModuleR p i (\m -> (\r -> (r,())) <$> (f m))

-- |Same as 'editModuleR', but the transformation is garunteed to succeed
editModuleR' :: Project
             -> ModuleInfo
             -> (Module -> (Module,a))
             -> Either ProjectError (Project,a)
editModuleR' p i f = editModuleR p i (return . f)

-- |Same as 'editModuleR'', but no extra result is produced by the transformation
editModule' :: Project
            -> ModuleInfo
            -> (Module -> Module)
            -> Either ProjectError Project
editModule' p i f = editModule p i (return . f)

-- |Add an import to a module
addImport :: Project
          -> ModuleInfo
          -> WithBody Import
          -> Either ProjectError (Project,ImportId)
addImport p mi i = editModuleR' p mi $ \m -> Module.addImport m i
-- |Remove an import from a module
--  This function fails if no matching import is found
removeImport :: Project
             -> ModuleInfo
             -> ImportId
             -> Either ProjectError Project
removeImport p mi i = editModule p mi $ \m -> Module.removeImport m i
-- |Set a module to export all of its symbols
exportAll :: Project
          -> ModuleInfo
          -> Either ProjectError Project
exportAll p mi = editModule' p mi $ \m -> Module.exportAll m
-- |Add an export to a module
addExport :: Project
          -> ModuleInfo
          -> WithBody Export
          -> Either ProjectError (Project, ExportId)
addExport p mi e = editModuleR' p mi $ \m -> Module.addExport m e
-- |Remove an exporty from a module
--  This function fails if no matching export is found
removeExport :: Project
             -> ModuleInfo
             -> ExportId
             -> Either ProjectError Project
removeExport p mi e = editModule p mi $ \m -> Module.removeExport m e
-- |Add a declaration to a module
addDeclaration :: Project 
               -> ModuleInfo 
               -> WithBody Declaration
               -> Either ProjectError Project
addDeclaration p i d = editModule' p i (`Module.addDeclaration` d)
-- |Remove a declaration from a module
--  This function fails if no matching declaration is found
removeDeclaration :: Project 
                  -> ModuleChild DeclarationInfo 
                  -> Either ProjectError Project
removeDeclaration p (ModuleChild i d)
  = editModule p i (`Module.removeDeclaration` d)

-- |Apply a transformation to a declaration in a project
editDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> (Declaration -> Either ProjectError Declaration)
                -> Either ProjectError Project
editDeclaration p (ModuleChild i di) f = do
    mi <- Module.info <$> getModule p i
    editModule p mi (\m -> Module.editDeclaration m di f)

-- |Same as `editDeclaration`, but the transformation is garunteed to succeed
editDeclaration' :: Project
                 -> ModuleChild DeclarationInfo
                 -> (Declaration -> Declaration)
                 -> Either ProjectError Project
editDeclaration' p m f = editDeclaration p m (return . f)

-- |Move a declaration from one module to another
moveDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> ModuleInfo
                -> Either ProjectError Project
moveDeclaration p c@(ModuleChild i di) i' = do
  (ModuleChild _ d) <- getDeclaration p c
  p' <- removeDeclaration p c
  addDeclaration p' i' d

-- |Get a declaration from a module
getDeclaration :: Project 
               -> ModuleChild DeclarationInfo 
               -> Either ProjectError (ModuleChild (WithBody Declaration))
getDeclaration p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration` di)

-- |Same as 'getDeclaration', but discards the body
getDeclaration' :: Project
                -> ModuleChild DeclarationInfo
                -> Either ProjectError (ModuleChild Declaration)
getDeclaration' p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration'` di)

