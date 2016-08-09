{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Ide3.Module.Internal where

import Data.Maybe
import Data.List (intercalate, find, intersperse)

import Data.Map (Map)
import qualified Data.Map as Map

import Ide3.OrderedMap (OrderedMap)
import qualified Ide3.OrderedMap as OMap

import Control.Monad
import Control.Monad.Trans.Except

import Ide3.Types.Internal

import Ide3.Env

import Ide3.Module.Parser (ExtractionResults(..))
import qualified Ide3.Module.Parser as Parser
import qualified Ide3.Import.Internal as Import
import qualified Ide3.Declaration as Declaration

instance ParamEnvClass Module DeclarationInfo (WithBody Declaration) (SolutionError u) where
    addChildT = addDeclaration
    removeChildT = removeDeclaration
    getChildT = getDeclaration
    setChildT = setDeclaration

instance ParamEnvClass Module ImportId (WithBody Import) (SolutionError u) where
    addChildT = addImport
    removeChildT = removeImport
    getChildT = getImport
    setChildT = setImport

instance ParamEnvClass Module ExportId (WithBody Export) (SolutionError u) where
    addChildT = addExport
    removeChildT = removeExport
    getChildT = getExport
    setChildT = setExport
    

-- | Get the identifying information from a module
info :: Module -> ModuleInfo
info = moduleInfo

-- | Create an empty module
empty :: Module
empty = Module
      { moduleInfo = (UnamedModule Nothing) 
      , moduleHeader = ""
      , modulePragmas = []
      , moduleImports = Map.empty 
      , moduleExports = Nothing
      , moduleDeclarations = OMap.empty
      }

-- | Create a new module from a ModuleInfo
new :: ModuleInfo -> Module
new i = Module 
      { moduleInfo = i 
      , moduleHeader = ""
      , modulePragmas = []
      , moduleImports = Map.empty 
      , moduleExports = Nothing
      , moduleDeclarations = OMap.empty
      }

addDeclaration :: Monad m 
               => DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult m u Module
addDeclaration di d m = case OMap.lookup di $ moduleDeclarations m of
    Just _ -> throwE $ DuplicateDeclaration (info m) di "Module.addDeclaration"
    Nothing -> return
        $ m 
        { moduleDeclarations = OMap.insert di d $ moduleDeclarations m 
        }

removeDeclaration :: Monad m
                  => DeclarationInfo
                  -> Module
                  -> SolutionResult m u (WithBody Declaration, Module)
removeDeclaration di m = case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.removeDeclaration"
    Just d -> return 
        ( d
        , m
        { moduleDeclarations = OMap.delete di $ moduleDeclarations m 
        }
        )

getDeclaration :: Monad m
               => DeclarationInfo
               -> Module
               -> SolutionResult m u (WithBody Declaration)
getDeclaration di m = case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.getDeclaration"
    Just d -> return d

setDeclaration :: Monad m
               => DeclarationInfo
               -> DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult m u Module
setDeclaration di di' d' m = case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.setDeclaration"
    Just _ -> return $ m
        { moduleDeclarations
            = OMap.modifyKey di di'
            $ OMap.insert' di' d'
            $ moduleDeclarations m
        }

addImport :: Monad m 
          => ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult m u Module
addImport ii i m = case Map.lookup ii $ moduleImports m of
    Just _ -> throwE $ InternalError "Duplicate import id" "Module.addImport"
    Nothing -> return $ m{ moduleImports = Map.insert ii i $ moduleImports m }

removeImport :: Monad m
             => ImportId
             -> Module
             -> SolutionResult m u (WithBody Import, Module)
removeImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.removeImport"
    Just i -> return (i, m{ moduleImports = Map.delete ii $ moduleImports m })

getImport :: Monad m
          => ImportId
          -> Module
          -> SolutionResult m u (WithBody Import)
getImport ii m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.getImport"
    Just i -> return i

setImport :: Monad m
          => ImportId
          -> ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult m u Module
setImport ii ii' i' m = case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.setImport"
    Just _ -> return $ m
        { moduleImports
            = Map.insert ii' i'
            $ Map.delete ii
            $ moduleImports m
        }

addExport :: Monad m 
          => ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult m u Module
addExport ei e m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Just _ -> throwE $ InternalError "Duplicate export id" "Module.addExport"
        Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e es }
    Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e Map.empty }

removeExport :: Monad m
             => ExportId
             -> Module
             -> SolutionResult m u (WithBody Export, Module)
removeExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't remove export from an export all" "Module.removeExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
        Just e -> return (e, m{ moduleExports = Just $ Map.delete ei es })

getExport :: Monad m
          => ExportId
          -> Module
          -> SolutionResult m u (WithBody Export)
getExport ei m = case moduleExports m of
    Nothing -> throwE $ InvalidOperation "Can't get export from an export all" "Module.getExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (info m) ei "Module.getExport"
        Just e -> return e

setExport :: Monad m
          => ExportId
          -> ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult m u Module
setExport ei ei' e' m = case moduleExports m of
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InternalError "Tried to set an export in an export all" "Module.setExport"
        Just _ -> return $ m
            { moduleExports
                = Just
                $ Map.insert ei' e'
                $ Map.delete ei
                  es
            }
    Nothing -> return $ m { moduleExports = Just $ Map.fromList [(ei',e')] }


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
    Right (Extracted minfo header pragmas exports imports decls) 
            -> Right (Module newInfo header pragmas imports' exports' decls', eids, iids)
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
        decls' = OMap.fromList $ zip (map (Declaration.info . item) decls) decls
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
getDeclarations = OMap.elems . moduleDeclarations


-- |Get the exports from a module
getExports :: Module -> Maybe (Map ExportId (WithBody Export))
getExports = moduleExports

getExportIds :: Module -> Maybe [ExportId]
getExportIds = liftM Map.keys . moduleExports

getImportIds :: Module -> [ImportId]
getImportIds = Map.keys . moduleImports


makeExportList :: [String] -> String
makeExportList exportBodies@[_,_,_,_] = "(" ++ intercalate "," exportBodies ++ ") where"
makeExportList exportBodies
    = unlines 
    $ map ("\t" ++)
        $ "(" 
        : map (", " ++) exportBodies 
        ++ [") where"]

-- | Produce the header (module name and export list) for a module
getHeaderText :: Module -> String
getHeaderText m = maybe withNoExportList withExportList $ moduleExports m
  where
    ModuleInfo (Symbol name) = info m
    withNoExportList = "module " ++ name ++ " where"
    withExportList es = "module " ++ name ++ makeExportList exportBodies
      where
        exportBodies = bodies $ Map.elems es

splitOver :: (a -> a -> Bool) -> [a] -> [[a]]
splitOver _ [] = []
splitOver f xs = go xs [] []
  where
    go [] ys zs = reverse (reverse ys : zs)
    go (x:xs) [] zs = go xs [x] zs
    go (x:xs) (y:ys) zs
        | f x y = go xs [x] (reverse (y : ys) : zs)
        | otherwise = go xs (x:y:ys) zs
    

spaceImports :: [WithBody Import] -> [String]
spaceImports is = concatMap ((++[""]) . bodies) partitionedImports
  where
    partitionedImports = flip splitOver is $ \i1 i2 -> not $ Import.commonPath (item i1) (item i2)

-- | Reconstruct the source code from a Module
toFile :: Module -> String
toFile m = intercalate "\n" parts
  where
    headerComment = moduleHeader m
    pragmas = modulePragmas m
    header = getHeaderText m
    imports = spaceImports $ Map.elems $ moduleImports m
    declarations = intersperse "" $ bodies $ OMap.elems $ moduleDeclarations m
    parts = headerComment : "" : pragmas ++ header : "" : imports ++ declarations

-- | Find all modifiers of a given symbol in the module
modifiersOf :: Symbol -> Module -> [ModuleChild DeclarationInfo]
modifiersOf s m
  = map (qualify m . Declaration.info . item) 
  . OMap.elems
  . OMap.filter ((`Declaration.affectsSymbol` s) . item)
  $ moduleDeclarations m

-- | Tag a value as belonging to this module
qualify :: Module -> a -> ModuleChild a
qualify = ModuleChild . moduleInfo



-- | Find all of the symbols that are created within this module
allSymbols :: Module -> [ModuleChild Symbol]
allSymbols m
  = concatMap (map (qualify m) . Declaration.symbolsProvided . item) $ moduleDeclarations m

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

nextId :: (Enum k, Ord k) => k -> Map k v -> k
nextId default_ m = succ $ maximum (pred default_ : Map.keys m)

-- | Get the next value to use for an ImportId
nextImportId :: Module -> ImportId
nextImportId = nextId 0 . moduleImports

-- | Get the next value to use as an ExportId
nextExportId :: Module -> ExportId
nextExportId = maybe 0 (nextId 0) . moduleExports
  
{-
-- | Test if a module has a matching declaration
hasDeclarationInfo :: Module -> DeclarationInfo -> Bool
hasDeclarationInfo m di = case getDeclaration m di of
    Right _ -> True
    Left  _ -> False
-}
