module Ide3.Project where

import qualified Data.Map as Map
import Data.Map.Strict ( Map )

import Ide3.Types
import qualified Ide3.Module as Module

empty :: Project
empty = Project ProjectInfo Map.empty BuildInfo

new :: ProjectInfo -> Project
new i = Project i Map.empty BuildInfo

modifiersOf :: Symbol -> Project -> [ModuleChild DeclarationInfo]
modifiersOf s (Project _ ms _) = concatMap (Module.modifiersOf s) ms

allDeclarations :: Project -> [ModuleChild DeclarationInfo]
allDeclarations (Project _ ms _) = concatMap Module.allDeclarations ms

allSymbols :: Project -> [ModuleChild Symbol]
allSymbols (Project _ ms _) = concatMap Module.allSymbols ms

addModule :: Project -> Module -> Either ProjectError Project
addModule (Project i ms b) m@(Module i' _ _ _)
  = case Map.lookup i' ms of
    Just _ -> Left $ "Project.addModule: " ++ (show i') ++ " Is already an existing module"
    Nothing -> Right $ Project i (Map.insert i' m ms) b

createModule :: Project -> ModuleInfo -> Either ProjectError Project
createModule p i = addModule p (Module.new i)

getModule :: Project -> ModuleInfo -> Either ProjectError Module
getModule (Project _ ms _) i = case Map.lookup i ms of
    Just m -> Right m
    Nothing -> Left $ "Project.getModule: " ++ (show i) ++ " did not match any modules"

hasModuleInfo :: Project -> ModuleInfo -> Bool
hasModuleInfo p m = case getModule p m of
    Right _ -> True
    Left _ -> False

hasModule :: Project -> Module -> Bool
hasModule p (Module i _ _ _) = hasModuleInfo p i

removeModule :: Project -> ModuleInfo -> Either ProjectError Project
removeModule p@(Project pi ms b) i
    | p `hasModuleInfo` i = Right $ Project pi ms' b
    | otherwise           = Left $ "Project.removeModule: " ++ (show i) ++ " did not match any modules"
  where
    ms' = Map.delete i ms

editModuleR :: Project 
            -> ModuleInfo
            -> (Module -> Either ProjectError (Module,a))
            -> Either ProjectError (Project,a)
editModuleR p@(Project pi ms b) i f = do
    m <- getModule p i
    (m',x) <- f m
    let ms' = Map.insert i m' ms
    return $ (Project pi ms' b, x)

editModule :: Project
           -> ModuleInfo
           -> (Module -> Either ProjectError Module)
           -> Either ProjectError Project
editModule p i f = fst <$> editModuleR p i (\m -> (\r -> (r,())) <$> (f m))

editModuleR' :: Project
             -> ModuleInfo
             -> (Module -> (Module,a))
             -> Either ProjectError (Project,a)
editModuleR' p i f = editModuleR p i (return . f)

editModule' :: Project
            -> ModuleInfo
            -> (Module -> Module)
            -> Either ProjectError Project
editModule' p i f = editModule p i (return . f)

addImport p mi i = editModuleR' p mi $ \m -> Module.addImport m i
removeImport p mi i = editModule p mi $ \m -> Module.removeImport m i
exportAll p mi = editModule' p mi $ \m -> Module.exportAll m
addExport p mi e = editModuleR' p mi $ \m -> Module.addExport m e
removeExport p mi e = editModule p mi $ \m -> Module.removeExport m e
    
addDeclaration :: Project 
               -> ModuleInfo 
               -> WithBody Declaration
               -> Either ProjectError Project
addDeclaration p i d = editModule' p i (`Module.addDeclaration` d)

removeDeclaration :: Project 
                  -> ModuleChild DeclarationInfo 
                  -> Either ProjectError Project
removeDeclaration p (ModuleChild i d)
  = editModule p i (`Module.removeDeclaration` d)

editDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> (Declaration -> Either ProjectError Declaration)
                -> Either ProjectError Project
editDeclaration p (ModuleChild i di) f = do
    mi <- Module.info <$> getModule p i
    editModule p mi (\m -> Module.editDeclaration m di f)

editDeclaration' p m f = editDeclaration p m (return . f)

moveDeclaration :: Project 
                -> ModuleChild DeclarationInfo
                -> ModuleInfo
                -> Either ProjectError Project
moveDeclaration p c@(ModuleChild i di) i' = do
  (ModuleChild _ d) <- getDeclaration p c
  p' <- removeDeclaration p c
  addDeclaration p' i' d

getDeclaration :: Project 
               -> ModuleChild DeclarationInfo 
               -> Either ProjectError (ModuleChild (WithBody Declaration))
getDeclaration p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration` di)

getDeclaration' :: Project
                -> ModuleChild DeclarationInfo
                -> Either ProjectError (ModuleChild Declaration)
getDeclaration' p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration'` di)

