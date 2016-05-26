{-|
Module      : Ide3.Module
Description : Top level operations on the Modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

This module contains the operations, convienence functions, and simple queries
for working with the Module data type.
-}
module Ide3.Module where

import Data.Maybe
import Data.List (intercalate, find)

import Control.Monad.Trans.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Ide3.Types
import Ide3.Monad (ProjectM, ProjectResult)
import qualified Ide3.Declaration as Declaration
import qualified Ide3.Export as Export
import qualified Ide3.Import as Import

import Ide3.Module.Parser (ExtractionResults(..))
import qualified Ide3.Module.Parser as Parser

-- |Get the identifying information from a module
info :: Module -> ModuleInfo
info (Module i _ _ _ _) = i

-- |Create an empty module
empty :: Module
empty = Module (UnamedModule Nothing) [] Map.empty Nothing Map.empty

-- |Create a new module from a ModuleInfo
new :: ModuleInfo -> Module
new i = Module i [] Map.empty Nothing Map.empty

-- |Similar to foldl, but each fold produces an extra result, the list of which
--  is returned along with the final fold
foldlRes :: (b -> a -> (b,c)) -> b -> [a] -> (b,[c])
foldlRes _ x [] = (x,[])
foldlRes f x (y:ys) =
    let (x',z) = f x y
        (x'', zs) = foldlRes f x' ys
    in (x'',z:zs)
                    
-- |Parse a complete module from a string, returning the Module data structure
--  created, along with each of the export and import ids created
parse :: String -> Maybe FilePath -> Either (ProjectError u) (Module,[ExportId],[ImportId])
parse s p = case Parser.parse s p of
    Right (Extracted minfo pragmas exports imports decls) -> Right (withDecls, eids, iids)
      where
        newModule = new $ case minfo of
            UnamedModule Nothing -> UnamedModule p
            x -> x
        withPragmas = foldl addPragma newModule pragmas
        (withExports,eids) = case exports of
            Just exportList -> foldlRes addExport withPragmas exportList
            Nothing -> (Ide3.Module.exportAll withPragmas,[])
        (withImports,iids) = foldlRes addImport withExports imports
        withDecls = foldl addDeclaration withImports decls
    Left msg -> Left msg

-- |Get the imports from a module
getImports :: Module -> [WithBody Import]
getImports (Module _ _ is _ _) = Map.elems is

getImport :: Module -> ImportId -> Either (ProjectError u) (WithBody Import)
getImport (Module mi _ is _ _) iid = case Map.lookup iid is of
    Just i -> Right i
    Nothing -> Left $ InvalidImportId mi iid "Module.getImport"

getPragmas :: Module -> [Pragma]
getPragmas (Module _ ps _ _ _) = ps

-- |Get the declarations from a module
getDeclarations :: Module -> [WithBody Declaration]
getDeclarations (Module _ _ _ _ ds) = Map.elems ds

-- |Get the exports from a module
getExports :: Module -> Maybe (Map ExportId (WithBody Export))
--getExports (Module _ _ _ Nothing _) = []
--getExports (Module _ _ _ (Just es) _) = Map.elems es
getExports (Module _ _ _ es _) = es

getExport :: Module -> ExportId -> Either (ProjectError u) (WithBody Export)
getExport (Module mi _ _ (Just es) _) eid = case Map.lookup eid es of
    Just e -> Right e
    Nothing -> Left $ InvalidExportId mi eid "Module.getExport"
getExport (Module _ _ _ Nothing _) _ = Left $ InvalidOperation "Can't get export from an export all" "Module.getExport"

getExportIds :: Module -> Maybe [ExportId]
getExportIds (Module _ _ _ es _) = Map.keys <$> es

getImportIds :: Module -> [ImportId]
getImportIds (Module _ _ is _ _) = Map.keys is


-- |Produce the header (module name and export list) for a module
getHeaderText :: Module -> String
getHeaderText m = case (map (body . snd) . Map.toList) <$> getExports m of
    Nothing -> "module " ++ name ++ " where"
    Just es -> "module " ++ name ++ "(" ++ intercalate "," es ++ ") where"
  where
    ModuleInfo (Symbol name) = info m

-- |Reconstruct the source code from a Module
toFile :: Module -> String
toFile m = intercalate "\n" parts
  where
    pragmas = getPragmas m
    header = getHeaderText m
    imports = bodies $ getImports m
    declarations = bodies $ getDeclarations m
    parts = pragmas ++ header : imports ++ declarations

-- |Find all modifiers of a given symbol in the module
modifiersOf :: Symbol -> Module -> [ModuleChild DeclarationInfo]
modifiersOf s m@(Module _ _ _ _ ds)
  = map (qualify m . Declaration.info . item) 
  . Map.elems
  . Map.filter ((`Declaration.affectsSymbol` s) . item)
  $ ds

-- |Tag a value as belonging to this module
qualify :: Module -> a -> ModuleChild a
qualify (Module i _ _ _ _) = ModuleChild i

-- |Within the context of a project, find all of the symbols this module exports
--  This requires the project context as modules may export other modules,
--      necessitating finding what symbols they export, and so on
exportedSymbols :: ProjectM m => Module -> ProjectResult m u [ModuleChild Symbol]
exportedSymbols m@(Module _ _ _ Nothing _) = return $ allSymbols m
exportedSymbols m@(Module _ _ _ (Just es) _) = do
    syms <- concat <$> mapM (Export.symbolsProvided m . item) es
    let qualSyms = map (qualify m) syms
    return qualSyms

-- | Within the context of a project, find all of the symbosl being imported by
-- a module
importedSymbols :: ProjectM m => Module -> ProjectResult m u [Symbol]
importedSymbols (Module _ _ is _ _)
    = concat <$> mapM (Import.symbolsProvided . item) is


-- |Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
internalSymbols :: ProjectM m => Module -> ProjectResult m u [Symbol]
internalSymbols m@(Module _ _ _ _ ds) = do
    importSyms <- importedSymbols m
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) ds

-- |Find all of the symbols that are created within this module
allSymbols :: Module -> [ModuleChild Symbol]
allSymbols m@(Module _ _ _ _ ds)
  = concatMap (map (qualify m) . Declaration.symbolsProvided . item) ds

-- |Apply a transformation to each item in a list, then find the first item
--  which did not fail the transformation, and the result
search :: (a -> Maybe b) -> [a] -> Maybe (a,b)
search f xs = case find (isJust . snd) $ map toPair xs of
    Just (x,Just y) -> Just (x,y)
    _ -> Nothing
  where
    toPair x = (x,f x)

--  |Same as 'search', but the transformation runs in a monad
searchM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a,b))
searchM f xs = do
    r <- find (isJust . snd) <$> mapM toPair xs 
    case r of
        Just (x,Just y) -> return $ Just (x,y)
        _ -> return Nothing
  where
    toPair x = do
        y <- f x
        return (x,y)

-- |Given a sub-symbol, (such as a data constructor or a class method), find
--  the parent symbol and its siblings
--  If successful, the list will contain the parent symbol as its head, and the
--      siblings as the tail. The symbol provided will not be an item in the list
--  If the symbol is imported, it will be tagged as such
symbolTree :: ProjectM m => Module -> Symbol -> ProjectResult m u [ModuleChild Symbol]
symbolTree m sym = do
    let declarations = items $ getDeclarations m
        declSearchResult = search (`Declaration.otherSymbols`sym) declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (qualify m) syms
        Nothing -> do
            let imports = items $ getImports m
            importSearchResult <- searchM (`Import.otherSymbols` sym) imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- Import.symbolTree i sym
                    return $ map (qualify m) otherSyms
                Nothing -> throwE $ SymbolNotFound (info m) sym "Module.symbolTree"

-- |Get a list of all declarations in a module
allDeclarations :: Module -> [ModuleChild DeclarationInfo]
allDeclarations m@(Module _ _ _  _ ds)
  = map (qualify m . Declaration.info . item) . Map.elems $ ds

-- |Test if this module matches a ModuleInfo
infoMatches :: Module -> ModuleInfo -> Bool
infoMatches (Module i _ _ _ _) i' = i == i'

-- |Get the next value to use for an ImportId
nextImportId :: Module -> ImportId
nextImportId (Module _ _ is _ _) = 1 + maximum (-1 : Map.keys is)

addPragma :: Module -> Pragma -> Module
addPragma (Module mi ps is es ds) p = Module mi (p:ps) is es ds

-- |Add an import to a module
addImport :: Module -> WithBody Import -> (Module, ImportId)
addImport m@(Module mi ps is es ds) i = (Module mi ps (Map.insert iid i is) es ds, iid)
    where
        iid = nextImportId m

-- |Remove an import from a module
--  This function fails if no matching import is found
removeImport :: Module -> ImportId -> Either (ProjectError u) Module
removeImport (Module mi ps is es ds) i
    = case Map.lookup i is of
        Just _ -> Right $ Module mi ps (Map.delete i is) es ds
        Nothing -> Left $ InvalidImportId mi i "Module.removeImport"

-- |Test if a module imports another
importsModule :: Module -> Symbol -> Bool
importsModule m sym = sym `elem` map Import.moduleName (items $ getImports m)

-- |Set a module to export all of its symbols
exportAll :: Module -> Module
exportAll (Module mi ps is _ ds) = Module mi ps is Nothing ds

-- |Get the next value to use as an ExportId
nextExportId :: Module -> ExportId
nextExportId (Module _ _ _ Nothing _) = 0
nextExportId (Module _ _ _ (Just es) _) = 1 + maximum (-1 : Map.keys es)

-- |Add an export to a module
addExport :: Module -> WithBody Export -> (Module,ExportId)
addExport m@(Module mi ps is es ds) e = (Module mi ps is es' ds,nextId)
  where
    nextId = nextExportId m
    es' = case es of
        Nothing -> Just $ Map.singleton nextId e
        Just el -> Just $ Map.insert nextId e el

-- |Remove an export from a module
--  This function fails if no matching export is found
removeExport :: Module -> ExportId -> Either (ProjectError u) Module
removeExport (Module _ _ _ Nothing _) _ = Left $ InvalidOperation "Can't remove an export from an export all" "Module.removeExport"
removeExport (Module mi ps is (Just es) ds) e
    = Right $ Module mi ps is (Just $ e `Map.delete` es) ds

exportNothing :: Module -> Module
exportNothing (Module mi ps is _ ds) = Module mi ps is (Just Map.empty) ds

-- |Add a declaration to a module
addDeclaration :: Module -> WithBody Declaration -> Module
addDeclaration (Module i ps is es ds) d = Module i ps is es ds'
  where
    di = Declaration.info . item $ d
    ds' = Map.insert di d ds
  
-- |Test if a module has a matching declaration
hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Right _ -> True
    Left  _ -> False

-- |Remove a declaration from a module
--  This function fails if no matching declaration is found
removeDeclaration :: Module -> DeclarationInfo -> Either (ProjectError u) Module
removeDeclaration m@(Module i ps is es ds) di
    | m `hasDeclarationInfo` di = Right $ Module i ps is es ds'
    | otherwise                 = Left $ DeclarationNotFound (info m) di "Module.removeDeclaration"
  where
    ds' = Map.delete di ds

-- |Apply a transformation to a declaration in a module
editDeclaration :: Module 
                -> DeclarationInfo
                -> (Declaration -> Either (ProjectError u) (WithBody Declaration))
                -> Either (ProjectError u) Module
editDeclaration m@(Module i ps is es ds) di f = do
    (ModuleChild _ (WithBody d _)) <- getDeclaration m di
    d' <- f d
    let ds' = Map.insert di d' ds
    return $ Module i ps is es ds'

-- |Same as 'editDeclaration', but the transformation is garunteed to suceed
editDeclaration' :: Module
                 -> DeclarationInfo
                 -> (Declaration -> WithBody Declaration)
                 -> Either (ProjectError u) Module
editDeclaration' m d f = editDeclaration m d (return . f)

-- |Get a declaration from a module
getDeclaration :: Module
               -> DeclarationInfo
               -> Either (ProjectError u) (ModuleChild (WithBody Declaration))
getDeclaration m@(Module _ _ _ _ ds) di = case Map.lookup di ds of
        Just d -> Right $ qualify m d
        Nothing -> Left $ DeclarationNotFound (info m) di "Module.getDeclaration"

-- |Same as 'getDeclaration', but the body is discarded
getDeclaration' :: Module
                -> DeclarationInfo
                -> Either (ProjectError u) (ModuleChild Declaration)
getDeclaration' m di = do
    (ModuleChild i (WithBody d _)) <- getDeclaration m di
    return $ ModuleChild i d
