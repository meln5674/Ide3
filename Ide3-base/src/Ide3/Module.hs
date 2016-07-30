{-|
Module      : Ide3.Module
Description : Queries over modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX

-}

module Ide3.Module 
    ( module Ide3.Module
    , module Ide3.Module.Internal
    ) where

import Data.Maybe
import Data.List (intercalate, find)

import Data.Map (Map)
import qualified Data.Map as Map

import Ide3.Module.Internal

import Ide3.Types
import qualified Ide3.Declaration as Declaration

import Ide3.Module.Parser (ExtractionResults(..))
import qualified Ide3.Module.Parser as Parser

-- | Parse a complete module from a string, returning the Module data structure
--  created, along with each of the export and import ids created
parse :: String -> Maybe FilePath -> Either (SolutionError u) (Module,[ExportId],[ImportId])
parse = parseUsing Parser.parse

-- | Parse a complete module from a string, returning the Module data structure
--  created, along with each of the export and import ids created
parseMain :: String -> Maybe FilePath -> Either (SolutionError u) (Module,[ExportId],[ImportId])
parseMain = parseUsing Parser.parseMain

parseUsing :: (String -> Maybe FilePath 
                      -> Either (SolutionError u) ExtractionResults)
           -> String 
           -> Maybe FilePath 
           -> Either (SolutionError u) (Module,[ExportId],[ImportId])
parseUsing parser s p = case parser s p of
    Right (Extracted minfo pragmas exports imports decls) 
            -> Right (Module newInfo pragmas imports' exports' decls', eids, iids)
      where
        newInfo = case minfo of
            UnamedModule Nothing -> UnamedModule p
            x -> x
        eids = case exports of
            Just exportList -> [0..length exportList]
            Nothing -> []
        exports' = case exports of
            Just exportList -> Just $ Map.fromList $ zip eids exportList
            Nothing -> Nothing
        iids = [0..length imports]
        imports' = Map.fromList $ zip iids imports
        decls' = Map.fromList $ zip (map (Declaration.info . item) decls) decls
    Left msg -> Left msg


-- |Get the imports from a module
getImports :: Module -> [WithBody Import]
getImports = Map.elems . moduleImports

{-
getImport :: Module -> ImportId -> Either (SolutionError u) (WithBody Import)
getImport m iid = case Map.lookup iid $ moduleImports m of
    Just i -> Right i
    Nothing -> Left $ InvalidImportId mi iid "Module.getImport"
-}

getPragmas :: Module -> [Pragma]
getPragmas = modulePragmas


-- |Get the declarations from a module
getDeclarations :: Module -> [WithBody Declaration]
getDeclarations = Map.elems . moduleDeclarations


-- |Get the exports from a module
getExports :: Module -> Maybe (Map ExportId (WithBody Export))
getExports = moduleExports

getExportIds :: Module -> Maybe [ExportId]
getExportIds (Module _ _ _ es _) = Map.keys <$> es

getImportIds :: Module -> [ImportId]
getImportIds (Module _ _ is _ _) = Map.keys is


-- | Produce the header (module name and export list) for a module
getHeaderText :: Module -> String
getHeaderText m = case (map (body . snd) . Map.toList) <$> moduleExports m of
    Nothing -> "module " ++ name ++ " where"
    Just es -> "module " ++ name ++ "(" ++ intercalate "," es ++ ") where"
  where
    ModuleInfo (Symbol name) = info m

-- | Reconstruct the source code from a Module
toFile :: Module -> String
toFile m = intercalate "\n" parts
  where
    pragmas = modulePragmas m
    header = getHeaderText m
    imports = bodies $ Map.elems $ moduleImports m
    declarations = bodies $ Map.elems $ moduleDeclarations m
    parts = pragmas ++ header : imports ++ declarations

-- | Find all modifiers of a given symbol in the module
modifiersOf :: Symbol -> Module -> [ModuleChild DeclarationInfo]
modifiersOf s m@(Module _ _ _ _ ds)
  = map (qualify m . Declaration.info . item) 
  . Map.elems
  . Map.filter ((`Declaration.affectsSymbol` s) . item)
  $ ds

-- | Tag a value as belonging to this module
qualify :: Module -> a -> ModuleChild a
qualify (Module i _ _ _ _) = ModuleChild i



-- | Find all of the symbols that are created within this module
allSymbols :: Module -> [ModuleChild Symbol]
allSymbols m@(Module _ _ _ _ ds)
  = concatMap (map (qualify m) . Declaration.symbolsProvided . item) ds

-- | Apply a transformation to each item in a list, then find the first item
--  which did not fail the transformation, and the result
search :: (a -> Maybe b) -> [a] -> Maybe (a,b)
search f xs = case find (isJust . snd) $ map toPair xs of
    Just (x,Just y) -> Just (x,y)
    _ -> Nothing
  where
    toPair x = (x,f x)

-- | Same as 'search', but the transformation runs in a monad
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

-- | Test if this module matches a ModuleInfo
infoMatches :: Module -> ModuleInfo -> Bool
infoMatches m mi = info m == mi


-- | Get the next value to use for an ImportId
nextImportId :: Module -> ImportId
nextImportId m = 1 + maximum (-1 : Map.keys (moduleImports m))

-- | Get the next value to use as an ExportId
nextExportId :: Module -> ExportId
nextExportId (Module _ _ _ Nothing _) = 0
nextExportId (Module _ _ _ (Just es) _) = 1 + maximum (-1 : Map.keys es)
  
{-
-- | Test if a module has a matching declaration
hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Right _ -> True
    Left  _ -> False
-}



