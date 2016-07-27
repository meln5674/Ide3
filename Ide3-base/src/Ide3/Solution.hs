module Ide3.Solution where

import qualified Data.Map as Map

import Control.Monad

import Ide3.Types
import qualified Ide3.Module as Module
import qualified Ide3.Project as Project

{-
-- |Create an empry Solution
empty :: Solution
empty = Solution (SolutionInfo "") Map.empty

-- |Create a new Solution from a SolutionInfo
new :: SolutionInfo -> Solution
new i = Solution i Map.empty

{-
-- |Given a symbol, find all of the declarations which modify it, tagged with
--  the module they are present in
modifiersOf :: Symbol -> Solution -> [ModuleChild DeclarationInfo]
modifiersOf s = concatMap (Project.modifiersOf s) . solutionProjects

-- |List every declaration in the solution, tagged with the modules they are
--  present in
allDeclarations :: Solution -> [ModuleChild DeclarationInfo]
allDeclarations = concatMap Project.allDeclarations . solutionProjects
-}
-- |List every declaration in the solution, tagged with the modules they are
--  present in
allDeclarationsIn :: Solution 
                  -> ProjectParam ModuleInfo
                  -> Either (SolutionError u) [ModuleChild DeclarationInfo]
allDeclarationsIn s arg
    = getProject s (getProjectInfo arg)
    >>= \p -> Project.allDeclarationsIn p $ getParam arg
{-
-- |List every symbol in the solution, tagged with the modules they are
--  present in
allSymbols :: Solution -> [ModuleChild Symbol]
allSymbols = concatMap Project.allSymbols . solutionProjects
-}

addProject :: Solution -> ProjectInfo -> Either (SolutionError u) Solution
addProject s pi = case Map.lookup pi $ solutionProjects s of
    Just _ -> Left $ DuplicateProject pi $ "Solution.addProject"
    Nothing -> Right $ s{solutionProjects = Map.insert pi (Project.new pi) $ solutionProjects s}

removeProject :: Solution -> ProjectInfo -> Either (SolutionError u) Solution
removeProject s pi = case Map.lookup pi $ solutionProjects s of
    Just _ -> Right $ s{solutionProjects = Map.delete pi $ solutionProjects s}
    Nothing -> Left $ ProjectNotFound pi $ "Solution.addProject"

getProjects :: Solution -> [ProjectInfo]
getProjects s = Map.keys $ solutionProjects s

getProject :: Solution -> ProjectInfo -> Either (SolutionError u) Project
getProject s pi = case Map.lookup pi $ solutionProjects s of
    Just p -> Right p
    Nothing -> Left $ DuplicateProject pi $ "Solution.addProject"


-- |Find the project which matches the provided ProjectInfo,
--  If found, apply the provided transformation.
--  If the transformation succeeds, replace the old project, and return the
--      extraneous result
editProjectR :: Solution 
            -> ProjectInfo
            -> (Project -> Either (SolutionError u) (Project,a))
            -> Either (SolutionError u) (Solution,a)
editProjectR s i f = do
    p <- getProject s i
    (p',x) <- f p
    let ps' = Map.insert i p' $ solutionProjects s
    return (s{solutionProjects = ps'}, x)
    
-- |Same as 'editModuleR', but no extra result is produced by the tranformation
editProject :: Solution
           -> ProjectInfo
           -> (Project -> Either (SolutionError u) Project)
           -> Either (SolutionError u) Solution
editProject p i f = fst <$> editProjectR p i (\m -> (\r -> (r,())) <$> f m)

-- |Same as 'editProjectR', but the transformation is garunteed to succeed
editProjectR' :: Solution
             -> ProjectInfo
             -> (Project -> (Project,a))
             -> Either (SolutionError u) (Solution,a)
editProjectR' p i f = editProjectR p i (return . f)

-- |Same as 'editProjectR'', but no extra result is produced by the transformation
editProject' :: Solution
            -> ProjectInfo
            -> (Project -> Project)
            -> Either (SolutionError u) Solution
editProject' p i f = editProject p i (return . f)

editProjectInfo :: Solution 
                -> ProjectInfo 
                -> ProjectInfo
                -> Either (SolutionError u) Solution
editProjectInfo s pi pi' = do
    p <- getProject s pi
    let p' = Project.editProjectInfo p $ const pi'
        ps' = Map.insert pi' p'
            $ Map.delete pi
            $ solutionProjects s
    return $ s{ solutionProjects = ps' }
    
-- |Get a list of every module in the solution
allModules :: Solution -> [ProjectParam ModuleInfo]
allModules 
    = concatMap (\p -> map (ProjectParam (projectInfo p)) $ Project.allModules p) 
    . solutionProjects

-- |Add a module to the solution
addModule :: Solution -> ProjectParam Module -> Either (SolutionError u) Solution
addModule s arg 
    = editProject s (getProjectInfo arg) 
    $ \p -> Project.addModule p $ getParam arg

-- |Create a new module from a ModuleInfo
createModule :: Solution -> ProjectParam ModuleInfo -> Either (SolutionError u) Solution
createModule s arg 
    = editProject s (getProjectInfo arg) 
    $ \p -> Project.addModule p $ Module.new $ getParam arg

addExternModule :: Solution -> ProjectParam ExternModule -> Either (SolutionError u) Solution
addExternModule s arg 
    = editProject s (getProjectInfo arg) 
    $ \p -> Project.addExternModule p $ getParam arg

-- |Retreive a module using its ModuleInfo
getModule :: Solution -> ProjectParam ModuleInfo -> Either (SolutionError u) Module
getModule s arg
    = getProject s (getProjectInfo arg)
    >>= \p -> Project.getModule p $ getParam arg

getExternModule :: Solution -> ProjectParam ModuleInfo -> Either (SolutionError u) ExternModule
getExternModule s arg
    = getProject s (getProjectInfo arg)
    >>= \p -> Project.getExternModule p $ getParam arg

editModule :: Solution 
           -> ProjectParam ModuleInfo 
           -> (Module -> Either (SolutionError u) Module)
           -> Either (SolutionError u) Solution
editModule s arg f
    = editProject s (getProjectInfo arg)
    $ \p -> Project.editModule p (getParam arg) f

removeModule :: Solution 
             -> ProjectParam ModuleInfo 
             -> Either (SolutionError u) Solution
removeModule s arg 
    = editProject s (getProjectInfo arg)
    $ \p -> Project.removeModule p $ getParam arg

addDeclaration :: Solution 
               -> ModuleParam (WithBody Declaration)
               -> Either (SolutionError u) Solution
addDeclaration s arg
    = editProject s (getProjectInfo arg)
    $ \p -> Project.addDeclaration p (getModuleInfo arg) (getParam arg)

getDeclaration :: Solution 
               -> ModuleParam DeclarationInfo 
               -> Either (SolutionError u) (ModuleChild (WithBody Declaration))
getDeclaration s arg 
    = getProject s (getProjectInfo arg)
    >>= \p -> Project.getDeclaration p $ ModuleChild (getModuleInfo arg) (getParam arg)

{-
-- |Determine if there is a module that matches the provided ModuleInfo
hasModuleInfo :: Solution -> ModuleInfo -> Bool
hasModuleInfo p m = hasLocalModuleInfo p m || hasExternModuleInfo p m

hasLocalModuleInfo :: Solution -> ModuleInfo -> Bool
hasLocalModuleInfo p m = case getModule p m of
    Right _ -> True
    Left _ -> False

hasExternModuleInfo :: Solution -> ModuleInfo -> Bool
hasExternModuleInfo p m = case getExternModule p m of
    Right _ -> True
    Left _ -> False
    
-- |Determine if there is a module which is exactly equivalent to a provided one
hasModule :: Solution -> Module -> Bool
hasModule p (Module i _ _ _ _) = hasModuleInfo p i

hasExternModule :: Solution -> ExternModule -> Bool
hasExternModule p (ExternModule i _) = hasModuleInfo p i

-- |Remove a module that has matching ModuleInfo
--  This function will fail if no matching module is found
removeModule :: Solution -> ModuleInfo -> Either (SolutionError u) Solution
removeModule p i
    | p `hasLocalModuleInfo` i = Right $ p{solutionModules = ms'} 
    | otherwise                = Left $ ModuleNotFound i "Solution.removeModule"
  where
    ms = solutionModules p
    ms' = Map.delete i ms

removeExternModule :: Solution -> ModuleInfo -> Either (SolutionError u) Solution
removeExternModule p i
    | p `hasExternModuleInfo` i = Right $ p{solutionExternModules = ms'} 
    | otherwise                 = Left $ ModuleNotFound i "Solution.removeModule"
  where
    ms = solutionExternModules p
    ms' = Map.delete i ms

-}
-- |Add an import to a module
addImport :: Solution
          -> ProjectInfo
          -> ModuleInfo
          -> WithBody Import
          -> Either (SolutionError u) (Solution,ImportId)
addImport s pi mi i = editProjectR s pi $ \p -> Project.addImport p mi i


-- |Remove an import from a module
--  This function fails if no matching import is found
removeImport :: Solution
             -> ProjectInfo
             -> ModuleInfo
             -> ImportId
             -> Either (SolutionError u) Solution
removeImport s pi mi i = editProject s pi $ \p -> Project.removeImport p mi i

getImport :: Solution
          -> ProjectInfo
          -> ModuleInfo
          -> ImportId
          -> Either (SolutionError u) (WithBody Import)
getImport s pi mi iid = getProject s pi >>= \p -> Project.getImport p mi iid


getImports :: Solution -> ProjectInfo -> ModuleInfo -> Either (SolutionError u) [ImportId]
getImports s pi mi = getProject s pi >>= \p -> Project.getImports p mi


-- |Set a module to export all of its symbols
exportAll :: Solution
          -> ProjectInfo
          -> ModuleInfo
          -> Either (SolutionError u) Solution
exportAll s pi mi = editProject s pi $ \p -> Project.exportAll p mi

-- |Add an export to a module
addExport :: Solution
          -> ProjectInfo
          -> ModuleInfo
          -> WithBody Export
          -> Either (SolutionError u) (Solution, ExportId)
addExport s pi mi e = editProjectR s pi $ \p -> Project.addExport p mi e

-- |Remove an exporty from a module
--  This function fails if no matching export is found
removeExport :: Solution
             -> ProjectInfo
             -> ModuleInfo
             -> ExportId
             -> Either (SolutionError u) Solution
removeExport s pi mi e = editProject s pi $ \p -> Project.removeExport p mi e


-- | Set a module to export nothing
-- This function failes if no matching module is found
exportNothing :: Solution
              -> ProjectInfo
              -> ModuleInfo
              -> Either (SolutionError u) Solution
exportNothing s pi mi = editProject s pi $ \p -> Project.exportNothing p mi

 
getExports :: Solution -> ProjectInfo -> ModuleInfo -> Either (SolutionError u) (Maybe [ExportId]) 
getExports s pi mi = getProject s pi >>= \p -> Project.getExports p mi

getExport :: Solution
          -> ProjectInfo
          -> ModuleInfo
          -> ExportId
          -> Either (SolutionError u) (WithBody Export)
getExport s pi mi eid = getProject s pi >>= \p -> Project.getExport p mi eid

{-
-- |Add a declaration to a module
addDeclaration :: Solution 
               -> ModuleInfo 
               -> WithBody Declaration
               -> Either (SolutionError u) Solution
addDeclaration p i d = editModule' p i (`Module.addDeclaration` d)
-}

-- |Remove a declaration from a module
--  This function fails if no matching declaration is found
removeDeclaration :: Solution 
                  -> ProjectInfo
                  -> ModuleChild DeclarationInfo 
                  -> Either (SolutionError u) Solution
removeDeclaration s pi c = editProject s pi $ \p -> Project.removeDeclaration p c

-- |Apply a transformation to a declaration in a solution
editDeclaration :: Solution 
                -> DeclarationParam
                    (Declaration -> Either (SolutionError u) (WithBody Declaration))
                -> Either (SolutionError u) (Solution,DeclarationInfo)
editDeclaration s arg
    = editProjectR s (getProjectInfo arg)
    $ \p -> Project.editDeclaration p c f

{-
-- |Same as `editDeclaration`, but the transformation is garunteed to succeed
editDeclaration' :: Solution
                 -> ModuleChild DeclarationInfo
                 -> (Declaration -> (WithBody Declaration))
                 -> Either (SolutionError u) (Solution,DeclarationInfo)
editDeclaration' p m f = editDeclaration p m (return . f)

-- |Move a declaration from one module to another
moveDeclaration :: Solution 
                -> ModuleChild DeclarationInfo
                -> ModuleInfo
                -> Either (SolutionError u) Solution
moveDeclaration p c i' = do
  (ModuleChild _ d) <- getDeclaration p c
  p' <- removeDeclaration p c
  addDeclaration p' i' d

-- |Get a declaration from a module
getDeclaration :: Solution 
               -> ModuleChild DeclarationInfo 
               -> Either (SolutionError u) (ModuleChild (WithBody Declaration))
getDeclaration p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration` di)

-- |Same as 'getDeclaration', but discards the body
getDeclaration' :: Solution
                -> ModuleChild DeclarationInfo
                -> Either (SolutionError u) (ModuleChild Declaration)
getDeclaration' p (ModuleChild i di)
  = getModule p i >>= (`Module.getDeclaration'` di)
-}

addPragma :: Solution -> ProjectInfo -> ModuleInfo -> Pragma -> Either (SolutionError u) Solution
addPragma s pi mi pr = editProject s pi $ \p -> Project.addPragma p mi pr

removePragma :: Solution -> ProjectInfo -> ModuleInfo -> Pragma -> Either (SolutionError u) Solution
removePragma s pi mi pr = editProject s pi $ \p -> Project.removePragma p mi pr

getPragmas :: Solution -> ProjectInfo -> ModuleInfo -> Either (SolutionError u) [Pragma]
getPragmas s pi mi = getProject s pi >>= \p -> Project.getPragmas p mi
-}
