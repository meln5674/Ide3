{-|
Module      : Ide3.Module.Internal
Description : Operations on modules
Copyright   : (c) Andrew Melnick, 2016

License     : BSD3
Maintainer  : meln5674@kettering.edu
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide3.Module.Internal 
    ( module Ide3.Module.Internal
    , Parser.parseAtLocation
    ) where

import Data.Monoid

import Data.Maybe
import Data.List (find, intersperse)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Ide3.OrderedMap as OMap

import Control.Monad.Trans.Except

import Ide3.Types.Internal
import Ide3.Types.State

import Ide3.Env

import Ide3.Module.Parser (ExtractionResults(..))
import qualified Ide3.Module.Parser as Parser
import qualified Ide3.Import.Internal as Import
import qualified Ide3.Declaration as Declaration

import Ide3.SrcLoc.Types

import Ide3.Utils.Parser

-- | Add, remove, retrieve, and overwrite declarations
instance ParamEnvClass Module 
                       DeclarationInfo 
                       (WithBody Declaration) 
                       (SolutionError u) where
    addChildT = addDeclaration
    removeChildT = removeDeclaration
    getChildT = getDeclaration
    setChildT = setDeclaration

-- | Add, remove, retrieve, and overwrite imports
instance ParamEnvClass Module ImportId (WithBody Import) (SolutionError u) where
    addChildT = addImport
    removeChildT = removeImport
    getChildT = getImport
    setChildT = setImport

-- | Add, remove, retrieve, and overwrite exports
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
      { moduleInfo = ModuleInfo $ Symbol "Main"
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

withParsable :: Monad m => Module -> SolutionResult u m a -> SolutionResult u m a
withParsable m@Module{} x = x
withParsable m _ = throwE $ InvalidOperation ("Module " ++ show (moduleInfo m) ++ " is not parsable") ""

withParsableF :: Monad m => (Module -> SolutionResult u m a) -> Module -> SolutionResult u m a
withParsableF f m@Module{} = f m
withParsableF _ m = throwE $ InvalidOperation ("Module " ++ show (moduleInfo m) ++ " is not parsable") ""

withParsableF' :: Monad m => (Module -> a) -> Module -> SolutionResult u m a
withParsableF' f m@Module{} = return $ f m
withParsableF' _ m = throwE $ InvalidOperation msg ""
  where
    msg = "Module " ++ show (moduleInfo m) 
                    ++ " is not parsable " 
                    ++ show (moduleErrorLoc m) 
                    ++ ": " 
                    ++ show (moduleErrorMsg m)

-- | Add a declaration to a module
addDeclaration :: Monad m 
               => DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult u m Module
addDeclaration di d m = withParsable m $ case OMap.lookup di $ moduleDeclarations m of
    Just _ -> throwE $ DuplicateDeclaration (info m) di "Module.addDeclaration"
    Nothing -> return
        $ m 
        { moduleDeclarations = OMap.insert di d $ moduleDeclarations m 
        }

-- | Remove a declaration from a module
removeDeclaration :: Monad m
                  => DeclarationInfo
                  -> Module
                  -> SolutionResult u m (WithBody Declaration, Module)
removeDeclaration di m = withParsable m $ case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE 
        $ DeclarationNotFound (info m) di "Module.removeDeclaration"
    Just d -> return 
        ( d
        , m { moduleDeclarations = OMap.delete di $ moduleDeclarations m }
        )

-- | Get a declaration from a module
getDeclaration :: Monad m
               => DeclarationInfo
               -> Module
               -> SolutionResult u m (WithBody Declaration)
getDeclaration di m = withParsable m $ case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.getDeclaration"
    Just d -> return d

-- | Update a declaration in a module
setDeclaration :: Monad m
               => DeclarationInfo
               -> DeclarationInfo
               -> WithBody Declaration
               -> Module
               -> SolutionResult u m Module
setDeclaration di di' d' m = withParsable m $ case OMap.lookup di $ moduleDeclarations m of
    Nothing -> throwE $ DeclarationNotFound (info m) di "Module.setDeclaration"
    Just _ -> return $ m
        { moduleDeclarations
            = OMap.insert' di' d'
            $ OMap.modifyKey di di'
            $ moduleDeclarations m
        }

-- | Add an import to a module
addImport :: Monad m 
          => ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult u m Module
addImport ii i m = withParsable m $ case Map.lookup ii $ moduleImports m of
    Just _ -> throwE $ InternalError "Duplicate import id" "Module.addImport"
    Nothing -> return $ m{ moduleImports = Map.insert ii i $ moduleImports m }

-- | Remove an import from a module
removeImport :: Monad m
             => ImportId
             -> Module
             -> SolutionResult u m (WithBody Import, Module)
removeImport ii m = withParsable m $ case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.removeImport"
    Just i -> return (i, m{ moduleImports = Map.delete ii $ moduleImports m })

-- | Get an import from a module
getImport :: Monad m
          => ImportId
          -> Module
          -> SolutionResult u m (WithBody Import)
getImport ii m = withParsable m $ case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.getImport"
    Just i -> return i

-- | Update an import in a module
setImport :: Monad m
          => ImportId
          -> ImportId
          -> WithBody Import
          -> Module
          -> SolutionResult u m Module
setImport ii ii' i' m = withParsable m $ case Map.lookup ii $ moduleImports m of
    Nothing -> throwE $ InvalidImportId (info m) ii "Module.setImport"
    Just _ -> return $ m
        { moduleImports
            = Map.insert ii' i'
            $ Map.delete ii
            $ moduleImports m
        }

-- | Add an export to a module
addExport :: Monad m 
          => ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult u m Module
addExport ei e m = withParsable m $ case moduleExports m of
    Just es -> case Map.lookup ei es of
        Just _ -> throwE
            $ InternalError "Duplicate export id" "Module.addExport"
        Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e es }
    Nothing -> return $ m{ moduleExports = Just $ Map.insert ei e Map.empty }

-- | Remove an export from a module
removeExport :: Monad m
             => ExportId
             -> Module
             -> SolutionResult u m (WithBody Export, Module)
removeExport ei m = withParsable m $ case moduleExports m of
    Nothing -> throwE
        $ InvalidOperation "Can't remove export from an export all" 
                           "Module.removeExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE
            $ InvalidExportId (moduleInfo m) ei "Module.removeExport"
        Just e -> return (e, m{ moduleExports = Just $ Map.delete ei es })

-- | Get an export from a module
getExport :: Monad m
          => ExportId
          -> Module
          -> SolutionResult u m (WithBody Export)
getExport ei m = withParsable m $ case moduleExports m of
    Nothing -> throwE
        $ InvalidOperation "Can't get export from an export all" 
                           "Module.getExport"
    Just es -> case Map.lookup ei es of
        Nothing -> throwE $ InvalidExportId (info m) ei "Module.getExport"
        Just e -> return e

-- | Update an export in a module
setExport :: Monad m
          => ExportId
          -> ExportId
          -> WithBody Export
          -> Module
          -> SolutionResult u m Module
setExport ei ei' e' m = withParsable m $ case moduleExports m of
    Just es -> case Map.lookup ei es of
        Nothing -> throwE
            $ InternalError "Tried to set an export in an export all" 
                            "Module.setExport"
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
parse :: String 
      -> Maybe FilePath 
      -> Maybe ModuleInfo
      -> Either (SolutionError u) (Module,[ExportId],[ImportId], Maybe (SrcLoc,String))
parse s path oldMi = (parseUsing $ \s' path' -> Parser.parse s' path' oldMi) s path

-- | Parse a complete module from a string, returning the Module data structure
--  created, along with each of the export and import ids created
parseMain :: String 
          -> Maybe FilePath 
          -> Either (SolutionError u) (Module,[ExportId],[ImportId], Maybe (SrcLoc,String))
parseMain = parseUsing Parser.parseMain

-- | Generalization of parse and parseMain
parseUsing :: (String -> Maybe FilePath 
                      -> Either (SolutionError u) 
                                (ExtractionResults SrcFileSpan)
              )
           -> String 
           -> Maybe FilePath 
           -> Either (SolutionError u) (Module,[ExportId],[ImportId], Maybe (SrcLoc,String))
parseUsing parser s p = case parser s p of
    Right extractResult@Extracted{} -> Right (module_, eids, iids, Nothing)
      where
        Extracted minfo header pragmas exports imports decls = extractResult
        newInfo = unAnn minfo
        header' = unAnn header
        pragmas' = map unAnn pragmas
        eids = maybe [] (map ExportId . enumFromTo 0 . length) exports
        exports' = fmap (Map.fromList . zip eids . map unAnn) exports 
        iids = map ImportId [0..length imports]
        imports' = Map.fromList $ zip iids $ map unAnn imports
        declInfoAndItem d = (Declaration.info $ item $ unAnn d, unAnn d)
        decls' = OMap.fromList $ map declInfoAndItem decls
        module_ = Module newInfo header' pragmas' imports' exports' decls'
    Right (Unparsable l msg mi contents) -> Right (unparsableModule, [], [], Just (l,msg))
       where
         unparsableModule = UnparsableModule mi contents l msg
    Left msg -> Left msg


-- | Get the imports from a module
getImports :: Monad m => Module -> SolutionResult u m [WithBody Import]
getImports = withParsableF' $ Map.elems . moduleImports

-- | Get the pragmas from a module
getPragmas :: Monad m => Module -> SolutionResult u m [Pragma]
getPragmas = withParsableF' $ modulePragmas

-- |Get the declarations from a module
getDeclarations :: Monad m => Module -> SolutionResult u m [WithBody Declaration]
getDeclarations = withParsableF' $ OMap.elems . moduleDeclarations

-- |Get the exports from a module
getExports :: Monad m => Module -> SolutionResult u m (Maybe (Map ExportId (WithBody Export)))
getExports = withParsableF' $ moduleExports

-- | Get the ids of all exports in a module structure, or signal that the module
--  exports everything
getExportIds :: Monad m => Module -> SolutionResult u m (Maybe [ExportId])
getExportIds = withParsableF' $ fmap Map.keys . moduleExports

-- | Get the ids of all imports in a module structure
getImportIds :: Monad m => Module -> SolutionResult u m [ImportId]
getImportIds = withParsableF' $ Map.keys . moduleImports

-- | Take the strings representing an export list and format them
makeExportList :: [Text] -> Text
makeExportList exportBodies
    | length exportBodies < 4 = listStart <> T.intercalate listSep exportBodies <> listEnd
    -- If there are 1, 2 or 3 exports, just put them in parenthesis
    -- If there are 4 or more, put them on separate lines
    | otherwise = T.unlines 
        $ blankLine
        : map indent ( (listStart <> head exportBodies)
                       : map addListSep (tail exportBodies)
                       ++ [listEnd] :: [Text]
                     )
  where
    blankLine :: Text
    blankLine = ""
    indent :: Text -> Text
    indent = ("    " <>)
    listStart :: Text
    listStart = "( "
    listSep :: Text
    listSep = ", "
    addListSep :: Text -> Text
    addListSep = (listSep <>)
    listEnd :: Text
    listEnd = ")"
    

-- | Produce the header (module name and export list) for a module
getHeaderText :: Module -> Text
getHeaderText m = maybe withNoExportList withExportList $ moduleExports m
  where
    ModuleInfo (Symbol name) = info m
    withNoExportList = headerStart <> name <> headerEnd
    withExportList es = headerStart
                      <> name 
                      <> makeExportList (bodies $ Map.elems es)
                      <> headerEnd
        
    headerStart = "module "
    headerEnd = " where"

-- | Take a list and a predicate over two elements at a time.
-- Split the list into a list of lists at the points where the predicate returns
-- true
-- i.e. splitOver (<) [5,4,1,2,10,8] == [[5,4,1],[2],[10,8]]
splitOver :: (a -> a -> Bool) -> [a] -> [[a]]
splitOver _ [] = []
splitOver f xs' = go xs' [] []
  where
    go [] ys zs = reverse (reverse ys : zs)
    go (x:xs) [] zs = go xs [x] zs
    go (x:xs) (y:ys) zs
        | f x y = go xs [x] (reverse (y : ys) : zs)
        | otherwise = go xs (x:y:ys) zs
    
-- | Take a list of items of which each can be turned into an import, split the
-- list into sub lists which have common module prefixes, apply a transformation
-- to each sublist, then concatenate them into a single list
spaceImports :: (a -> WithBody Import) -> ([a] -> [a]) -> [a] -> [a]
spaceImports f g is = concatMap g partitionedImports
  where
    partitionedImports = flip splitOver is $ \i1 i2 -> 
        not $ Import.commonPath (item $ f i1) (item $ f i2)

-- | Reconstruct the source code from a Module
toFile :: Module -> Text
toFile UnparsableModule { moduleContents = x } = x
toFile m = T.unlines $ concatMap annotation $ toAnnotatedFile m


-- | Annotates a string with the number of lines in it as well as the number of
-- characters in the final line
data Annotated x y = Annotated { annotated :: x, annotation :: y}

-- | Possibly a module item, annotated with the lines of text that item contains
type AnnotatedModuleItem = Annotated (Maybe ModuleItemKeyValue) [Text]

-- | Generate the annotated item for a module's header comment
annotateHeaderComment :: Module -> [AnnotatedModuleItem]
annotateHeaderComment m = [ Annotated (Just $ HeaderCommentKeyValue header) 
                                      (T.lines header)
                          ] 
  where
    header = moduleHeader m
-- | Generate the annotated items for a module's pragmas
annotatePragmas :: Module -> [AnnotatedModuleItem]
annotatePragmas m = flip map (modulePragmas m) 
    $ \p -> Annotated (Just $ PragmaKeyValue p) $ T.lines p

-- | Generate the annotated items for a module's header (module name, exports)
annotateHeader :: Module -> [AnnotatedModuleItem]
annotateHeader m = maybe withNoExportList withExportList $ moduleExports m
  where
    ModuleInfo (Symbol name) = info m
    withNoExportList = [Annotated Nothing [headerStart <> name <> headerEnd]]
    withExportList es = 
        Annotated Nothing [headerStart <> name]
        : case Map.toList es of
            [] -> [Annotated Nothing [indent $ exportListStart <> exportListEnd <> headerEnd]]
            [(eid,e)] -> [ Annotated (Just $ ExportKeyValue eid e) 
                                     [indent $ exportListStart <> body e]
                         , Annotated Nothing [indent $ exportListEnd <> headerEnd]
                         ]
            ((eid,e):es') -> Annotated (Just $ ExportKeyValue eid e) 
                                       [indent $ exportListStart <> body e]
                             
                             : map (uncurry annExport) es'
                             ++ [Annotated Nothing [indent $ exportListEnd <> headerEnd]]
    
    annExport eid' e' = Annotated (Just $ ExportKeyValue eid' e')
                                  [indent $ addExportListSep $ body e']
    headerStart = "module "
    headerEnd = " where"
    indent = ("    " <>)
    exportListStart = "( "
    exportListEnd = ")"
    exportListSep = ", "
    addExportListSep = (exportListSep <>)


-- | Generate the annotated items for a modules imports
annotateImports :: Module -> [AnnotatedModuleItem]
annotateImports m = flip map (Map.toList $ moduleImports m)
    $ \(iid, i) -> Annotated (Just $ ImportKeyValue iid i) $ T.lines $ body i

-- | Generate the annotated items for a modules declarations
annotateDeclarations :: Module -> [AnnotatedModuleItem]
annotateDeclarations m = flip map (OMap.toList $ moduleDeclarations m) 
    $ \(did, d) -> Annotated (Just $ DeclarationKeyValue did d) $ T.lines $ body d

-- | Generate an item with n blank lines
blankLines :: Int -> AnnotatedModuleItem
blankLines n = Annotated Nothing $ replicate n ""

-- | Insert an item with n blank lines between each item in a list
insertBlankLines :: Int -> [AnnotatedModuleItem] -> [AnnotatedModuleItem]
insertBlankLines n = intersperse (blankLines n)

-- | Generate all annotated items for a module
toAnnotatedFile :: Module -> [AnnotatedModuleItem]
toAnnotatedFile m 
    =  headerCommentLines 
    ++ blankLines 1
    :  pragmaLines 
    ++ headerLines 
    ++ blankLines 1
    :  spaceImports 
        (\(Annotated (Just (ImportKeyValue _ x)) _)-> x)
        (++[blankLines 1])
        importLines 
    ++ insertBlankLines 1 declarationLines
  where
    headerCommentLines = annotateHeaderComment m
    headerLines = annotateHeader m
    pragmaLines = annotatePragmas m
    importLines = annotateImports m
    declarationLines = annotateDeclarations m

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
allSymbols m = concatMap declSyms $ moduleDeclarations m
  where
    declSyms = map (qualify m) . Declaration.symbolsProvided . item

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

-- | Get the value one higher than the maximum key in a map, or a default value
-- if the map is empty
nextId :: (Enum k, Ord k) => k -> Map k v -> k
nextId default_ m = succ $ maximum (pred default_ : Map.keys m)

-- | Get the next value to use for an ImportId
nextImportId :: Module -> ImportId
nextImportId m@Module{} = nextId 0 $ moduleImports m
nextImportId _ = 0

-- | Get the next value to use as an ExportId
nextExportId :: Module -> ExportId
nextExportId m@Module{} = maybe 0 (nextId 0) $ moduleExports m
nextExportId _ = 0
