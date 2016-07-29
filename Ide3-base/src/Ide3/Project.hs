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
module Ide3.Project 
    ( module Ide3.Project
    , module Ide3.Project.Internal
    ) where

import qualified Data.Map as Map

import Control.Monad

import Ide3.Types
import qualified Ide3.Module as Module

import Ide3.Project.Internal (new, empty)

{-
-- |Create an empry project
empty :: Project
empty = Project (ProjectInfo "") Map.empty BuildInfo Map.empty

-- |Create a new project from a ProjectInfo
new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo Map.empty

editProjectInfo :: Project -> (ProjectInfo -> ProjectInfo) -> Project
editProjectInfo p f = p{ projectInfo = f $ projectInfo p }

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
allDeclarationsIn :: Project -> ModuleInfo -> Either (SolutionError u) [ModuleChild DeclarationInfo]
allDeclarationsIn p mi = liftM Module.allDeclarations $ getModule p mi

-- |List every symbol in the project, tagged with the modules they are
--  present in
allSymbols :: Project -> [ModuleChild Symbol]
allSymbols = concatMap Module.allSymbols . projectModules

-- |Get a list of every module in the project
allModules :: Project -> [ModuleInfo]
allModules = Map.keys . projectModules

-- |Add a module to the project
addModule :: Project -> Module -> Either (SolutionError u) Project
addModule p m@(Module i' _ _ _ _)
  = case Map.lookup i' ms of
    Just _ -> Left $ DuplicateModule i' "Project.addModule" 
    Nothing -> Right $ p{projectModules = Map.insert i' m ms} 
  where
    ms = projectModules p

-- |Create a new module from a ModuleInfo
createModule :: Project -> ModuleInfo -> Either (SolutionError u) Project
createModule p i = addModule p (Module.new i)

addExternModule :: Project -> ExternModule -> Either (SolutionError u) Project
addExternModule p m@(ExternModule i' _)
  = case Map.lookup i' ms of
    Just _ -> Left $ DuplicateModule i' "Project.addExternModule"
    Nothing -> Right $ p{projectExternModules = Map.insert i' m ms}
  where  
   ms = projectExternModules p

-- |Retreive a module using its ModuleInfo
getModule :: Project -> ProjectParam ModuleInfo -> Either (SolutionError u) Module
getModule p arg = case Map.lookup (getModuleInfo arg) ms of
    Just m -> Right m
    Nothing -> Left $ ModuleNotFound (getModuleInfo arg) "Project.getModule"
  where
    ms = projectModules p

getExternModule :: Project -> ModuleInfo -> Either (SolutionError u) ExternModule
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
removeModule :: Project -> ModuleInfo -> Either (SolutionError u) Project
removeModule p i
    | p `hasLocalModuleInfo` i = Right $ p{projectModules = ms'} 
    | otherwise                = Left $ ModuleNotFound i "Project.removeModule"
  where
    ms = projectModules p
    ms' = Map.delete i ms

removeExternModule :: Project -> ModuleInfo -> Either (SolutionError u) Project
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
            -> ModuleParam
                (Module -> Either (SolutionError u) (Module,a))
            -> Either (SolutionError u) (Project,a)
editModuleR p arg = do
    let i = getModuleInfo arg
        f = getParam arg
    m <- getModule p i
    (m',x) <- f m
    let ms' = Map.insert i m' $ projectModules p
    return (p{projectModules = ms'}, x)
    
-- |Same as 'editModuleR', but no extra result is produced by the tranformation
editModule :: Project
           -> ModuleParam (Module -> Either (SolutionError u) Module)
           -> Either (SolutionError u) Project
editModule p arg = do
    let arg' = setParam arg $ \m -> (\r -> (r,())) <$> (getParam arg) m
    (r,_) <- editModuleR p arg' 
    return r
    

-- |Same as 'editModuleR', but the transformation is garunteed to succeed
editModuleR' :: Project
             -> ModuleParam (Module -> (Module,a))
             -> Either (SolutionError u) (Project,a)
editModuleR' p arg = editModuleR p $ fmap (return .) arg

-- |Same as 'editModuleR'', but no extra result is produced by the transformation
editModule' :: Project
            -> ModuleParam (Module -> Module)
            -> Either (SolutionError u) Project
editModule' p arg = editModule p $ fmap (return .) arg

-- |Add an import to a module
addImport :: Project
          -> ModuleParam (WithBody Import)
          -> Either (SolutionError u) (Project,ImportId)
addImport p arg = editModuleR' p $ setParam arg $ \m -> Module.addImport m $ getParam arg
-- |Remove an import from a module
--  This function fails if no matching import is found
removeImport :: Project
             -> ModuleParam ImportId
             -> Either (SolutionError u) Project
removeImport p arg = editModule p $ setParam arg $ \m -> Module.removeImport m $ getParam arg

getImport :: Project
          -> ModuleInfo
          -> ImportId
          -> Either (SolutionError u) (WithBody Import)
getImport p mi iid = getModule p mi >>= \m -> Module.getImport m iid

getImports :: Project -> ModuleInfo -> Either (SolutionError u) [ImportId]
getImports p mi = getModule p mi >>= return . Module.getImportIds


-- |Set a module to export all of its symbols
exportAll :: Project
          -> ProjectParam ModuleInfo
          -> Either (SolutionError u) Project
exportAll p arg = editModule' p $ wrapProject arg $ \m -> Module.exportAll m
-- |Add an export to a module
addExport :: Project
          -> ModuleParam (WithBody Export)
          -> Either (SolutionError u) (Project, ExportId)
addExport p arg = editModuleR' p $ setParam arg $ \m -> Module.addExport m $ getParam arg
-- |Remove an exporty from a module
--  This function fails if no matching export is found
removeExport :: Project
             -> ModuleParam ExportId
             -> Either (SolutionError u) Project
removeExport p arg = editModule p $ setParam arg $ \m -> Module.removeExport m $ getParam arg

-- | Set a module to export nothing
-- This function failes if no matching module is found
exportNothing :: Project
              -> ProjectParam ModuleInfo
              -> Either (SolutionError u) Project
exportNothing p arg = editModule' p $ wrapProject arg $ Module.exportNothing

getExports :: Project -> ModuleInfo -> Either (SolutionError u) (Maybe [ExportId])
getExports p mi = getModule p mi >>= \m -> return (Module.getExportIds m)

getExport :: Project
          -> ModuleInfo
          -> ExportId
          -> Either (SolutionError u) (WithBody Export)
getExport p mi eid = getModule p mi >>= \m -> Module.getExport m eid

-- |Add a declaration to a module
addDeclaration :: Project 
               -> ModuleParam (WithBody Declaration)
               -> Either (SolutionError u) Project
addDeclaration p arg = editModule' p $ setParam arg $ \m -> Module.addDeclaration m $ getParam arg
-- |Remove a declaration from a module
--  This function fails if no matching declaration is found
removeDeclaration :: Project 
                  -> ModuleParam  DeclarationInfo 
                  -> Either (SolutionError u) Project
removeDeclaration p arg
  = editModule p $ setParam arg $ \m -> Module.removeDeclaration m $ getParam arg

-- |Apply a transformation to a declaration in a project
editDeclaration :: Project 
                -> DeclarationParam
                    (Declaration -> Either (SolutionError u) (WithBody Declaration))
                -> Either (SolutionError u) (Project,DeclarationInfo)
editDeclaration p arg = do
    mi <- Module.info <$> (getModule p $ getModuleInfo arg)
    editModuleR p $ setParam (unwrapModule arg) $ \m -> Module.editDeclaration m (getDeclarationInfo arg) (getParam arg)

-- |Same as `editDeclaration`, but the transformation is garunteed to succeed
editDeclaration' :: Project
                 -> DeclarationParam
                    (Declaration -> (WithBody Declaration))
                 -> Either (SolutionError u) (Project,DeclarationInfo)
editDeclaration' p arg = editDeclaration p $ fmap (return .) arg

-- |Move a declaration from one module to another
moveDeclaration :: Project 
                -> DeclarationParam ModuleInfo
                -> Either (SolutionError u) Project
moveDeclaration p arg = do
  (ModuleChild _ d) <- getDeclaration p $ unwrapModule arg
  p' <- removeDeclaration p $ unwrapModule arg
  addDeclaration p' $ setParam (unwrapModule arg) d

-- |Get a declaration from a module
getDeclaration :: Project 
               -> ModuleParam DeclarationInfo 
               -> Either (SolutionError u) (ModuleChild (WithBody Declaration))
getDeclaration p arg
  = getModule p (unwrapProject arg)
  >>= \m -> Module.getDeclaration m $ getParam arg

-- |Same as 'getDeclaration', but discards the body
getDeclaration' :: Project
                -> ModuleChild DeclarationInfo
                -> Either (SolutionError u) (ModuleChild Declaration)
getDeclaration' p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration'` di)

addPragma :: Project -> ModuleInfo -> Pragma -> Either (SolutionError u) Project
addPragma p mi pr = editModule' p mi (flip Module.addPragma pr)

removePragma :: Project -> ModuleInfo -> Pragma -> Either (SolutionError u) Project
removePragma p mi pr = editModule' p mi (flip Module.removePragma pr)

getPragmas :: Project -> ModuleInfo -> Either (SolutionError u) [Pragma]
getPragmas p mi = getModule p mi >>= return . Module.getPragmas
-}
