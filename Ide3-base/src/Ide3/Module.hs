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
import Data.List (intercalate, find, delete)

import Control.Monad.Trans.Except

import Data.Map (Map)
import qualified Data.Map as Map

import Ide3.Types
import Ide3.Monad (SolutionM)
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
parse :: String -> Maybe FilePath -> Either (SolutionError u) (Module,[ExportId],[ImportId])
parse s p = case Parser.parse s p of
    Right (Extracted minfo pragmas exports imports decls) 
            -> Right (Module newInfo pragmas imports' exports' decls', eids, iids)
      where
        newInfo = case minfo of
            UnamedModule Nothing -> UnamedModule p
            x -> x
        (exports',eids) = case exports of
            Just exportList -> let eids = [0..length exportList]
                               in  (Just $ Map.fromList $ zip eids exportList, eids)
            Nothing -> (Nothing,[])
        iids = [0..length imports]
        imports' = Map.fromList $ zip iids imports
        decls' = Map.fromList $ zip (map (Declaration.info . item) decls) decls
    Left msg -> Left msg

-- |Parse a complete module from a string, returning the Module data structure
--  created, along with each of the export and import ids created
parseMain :: String -> Maybe FilePath -> Either (SolutionError u) (Module,[ExportId],[ImportId])
parseMain s p = case Parser.parseMain s p of
    Right (Extracted minfo pragmas exports imports decls) 
            -> Right (Module newInfo pragmas imports' exports' decls', eids, iids)
      where
        newInfo = case minfo of
            UnamedModule Nothing -> UnamedModule p
            x -> x
        (exports',eids) = case exports of
            Just exportList -> let eids = [0..length exportList]
                               in  (Just $ Map.fromList $ zip eids exportList, eids)
            Nothing -> (Nothing,[])
        iids = [0..length imports]
        imports' = Map.fromList $ zip iids imports
        decls' = Map.fromList $ zip (map (Declaration.info . item) decls) decls
    Left msg -> Left msg

-- |Get the imports from a module
getImports :: Module -> [WithBody Import]
getImports (Module _ _ is _ _) = Map.elems is

getImport :: Module -> ImportId -> Either (SolutionError u) (WithBody Import)
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

getExport :: Module -> ExportId -> Either (SolutionError u) (WithBody Export)
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
exportedSymbols :: SolutionM m => ProjectInfo -> Module -> SolutionResult m u [ModuleChild Symbol]
exportedSymbols pi m = case m of
    (Module _ _ _ Nothing _) -> return $ allSymbols m
    (Module _ _ _ (Just es) _) -> do
        syms <- concat <$> mapM (Export.symbolsProvided pi m . item) es
        return $ map (qualify m) syms

-- | Within the context of a project, find all of the symbosl being imported by
-- a module
importedSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
importedSymbols pi m = concat <$> mapM providedBy imports
  where
    imports = moduleImports m
    providedBy = Import.symbolsProvided pi . item


-- |Within the context of a project, find all of the symbols which are visible
--  at the top level of this module 
internalSymbols :: SolutionM m 
                => ProjectInfo 
                -> Module 
                -> SolutionResult m u [Symbol]
internalSymbols pi m = do
    let decls = moduleDeclarations m
    importSyms <- importedSymbols pi m
    return $ importSyms ++ concatMap (Declaration.symbolsProvided . item) decls

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
symbolTree :: SolutionM m 
           => ProjectInfo
           -> Module 
           -> Symbol 
           -> SolutionResult m u [ModuleChild Symbol]
symbolTree pi m sym = do
    let declarations = items $ getDeclarations m
        declSearchResult = search (`Declaration.otherSymbols` sym) declarations
    case declSearchResult of
        Just (_,syms) -> return $ map (qualify m) syms
        Nothing -> do
            let imports = items $ getImports m
                othersFromImport x = Import.otherSymbols pi x sym
            importSearchResult <- searchM othersFromImport imports
            case importSearchResult of
                Just (i,_) -> do
                    otherSyms <- Import.symbolTree pi i sym
                    return $ map (qualify m) otherSyms
                Nothing -> throwE $ SymbolNotFound (info m) sym "Module.symbolTree"

-- |Test if this module matches a ModuleInfo
infoMatches :: Module -> ModuleInfo -> Bool
infoMatches (Module i _ _ _ _) i' = i == i'

-- |Get the next value to use for an ImportId
nextImportId :: Module -> ImportId
nextImportId (Module _ _ is _ _) = 1 + maximum (-1 : Map.keys is)

-- |Test if a module imports another
importsModule :: Module -> Symbol -> Bool
importsModule m sym = sym `elem` map Import.moduleName (items $ getImports m)

-- |Get the next value to use as an ExportId
nextExportId :: Module -> ExportId
nextExportId (Module _ _ _ Nothing _) = 0
nextExportId (Module _ _ _ (Just es) _) = 1 + maximum (-1 : Map.keys es)
  
{-
-- |Test if a module has a matching declaration
hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Right _ -> True
    Left  _ -> False
-}
